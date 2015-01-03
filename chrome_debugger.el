;;; chrome_debugger.el - Chrome Javascript Debugger for Emacs 

;; Author: Tobias Melcher <d031119@yahoo.de>
;; Keywords: Chrome, Debugger, Javascript
;; Version: 0.1
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Javascript debugger using the chrome debug websocket protocol. Debug
;; your Javascript code directly in emacs. All the known debugger features
;; from chrome like stepping, show stack, restart frame, object evaluation,
;; file recompilation, debugging of web workers are supported. 
;;
;; chrome needs to be started with following command line parameter
;; 	chrome --remote-debugging-port=9222
;; so that the debug port is open.
;;
;; Following interactive emacs commands are provided:
;; - chrome-connect                 - connect to running chrome instance
;; 				      (use tab completion to find the tab to be debugged)  
;; - chrome-disconnect              - disconnect from current debug session  
;; - chrome-add-breakpoint          - add a breakpoint
;; - chrome-remove-breakpoint       - remove a breakpoint
;; - chrome-step-into               - step into 
;; - chrome-step-over               - step over
;; - chrome-step-out                - step out 
;; - chrome-continue                - continue
;; - chrome-evaluate-expression     - evaluate expression at current point (completion by pressing tab)
;; - chrome-show-stack              - show stack list when in debugging mode
;;       commands inside show stack mode:
;;       - chrome-restart-stackframe - re-run stackframe at current point
;;       - chrome-console-navigate   - navigate to source for stackframe at current point
;; - chrome-show-console            - show console
;; - chrome-recompile-file          - recompiles current file and uploads to running chrome debug session
;; - chrome-disable-all-breakpoints - disable all breakpoints
;; - chrome-enable-all-breakpoints  - enables all breakpoints
;;
;; Dependencies:
;; - websocket
;; - json-reformat
;; both packages can be installed via M-x package-install 
;;
;; Status:
;; alpha

(require 'websocket)
(require 'json)
(require 'button)

(defun chrome-do-get (url) 
  (with-current-buffer (url-retrieve-synchronously url) 
    (buffer-string)
  )
)

(defun get-websocket-url-from-chrome ()
  (interactive)
  (let* ((str) (tabs) (titleList) (sel) (res) )
    (setq str (chrome-do-get "http://localhost:9222/json/list"))
    (setq str (substring str  (string-match "\\[" str) (+ 1 (string-match "\\]" str))))
    (let ((json-object-type 'hash-table))
      (setq tabs (json-read-from-string str))
      (dotimes (i (length tabs) titleList)  ;; last param is result
	(setq titleList (cons (gethash "title" (elt tabs i)) titleList))
	)
     (setq str (list (completing-read "Choose Tab to debug:" titleList)))
     (setq sel (car str)) ;; return the string
     ;;2nd loop
     (dotimes (i (length tabs) titleList)  ;; last param is result
       (if (string-equal sel (gethash "title" (elt tabs i)))
	   (setq res (gethash "webSocketDebuggerUrl" (elt tabs i)))
	 )
       )
     )
     res  ;; return webSocketDebuggerUrl
    )
  )

;;{"message":{"source":"deprecation","level":"warning","text":"'KeyboardEvent.keyLocation' is deprecated. Please use 'KeyboardEvent.location' instead.","timestamp":1416253908.63978,"type":"log","line":1477,"column":32,"url":"http://localhost:5040/ace/ace.js","stackTrace":[{"functionName":"normalizeCommandKeys","scriptId":"31","url":"http://localhost:5040/ace/ace.js","lineNumber":1477,"columnNumber":32},{"functionName":"","scriptId":"31","url":"http://localhost:5040/ace/ace.js","lineNumber":1507,"columnNumber":30}]}}
(defun add-console-message (params)
  (with-current-buffer (get-buffer-create "*Chrome Console*")
    (let* ((msg (gethash "message" params)) (stackEntry) (file) (line) (column) )
      (if (not (bound-and-true-p chrome-console-nav-mode))
	  (chrome-console-nav-mode) ;; enable nav mode
	  )
      ;; insert message text
      (goto-char (point-max))
      (insert (gethash "text" msg))
      (setq stackEntry (elt (gethash "stackTrace" msg) 0))
      (setq file (gethash "url" stackEntry))
      (setq line (gethash "lineNumber" stackEntry))
      (setq column (gethash "columnNumber" stackEntry))
      (insert (format " stack(%s:%i:%i)" file line column))
      (end-of-buffer) ;; jump to end ;; this doesn't work when focus is not in console buffer
      (insert "\n")
      )
    )
)

(defun chrome-console-navigate ()
  (interactive)
  (let ((line) (start) (end) (url) (lineNumber) (col))
    (setq line (thing-at-point 'line))
    (setq start (string-match "http:" line))
    (if (null start)
	;;maybe file:// url?
	(setq start (string-match "file:" line))
	)
    (setq end (string-match ")" line start))
    (setq line (substring line start end))
    (setq start (string-match ":" line))
    (setq start (string-match ":" line (1+ start)))
    (setq start (string-match ":" line (1+ start)))
    (setq end (string-match ":" line (1+ start)))
    (setq url (substring line 0 start))
    (setq lineNumber (substring line (1+ start) end))
    (setq col (substring line (1+ end) (length line)))
    (setq url (chrome-map-to-filename url))
    (other-window 1)
    (find-file url)
    (goto-char (point-min))
    (forward-line (- (string-to-int lineNumber) 1))
    (forward-char (- (string-to-int col) 1))
    )
)

(define-minor-mode chrome-console-nav-mode
  "chrome navigation mode. SPACE = navigate to source for given line"
  :lighter " chrome-nav"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'chrome-console-navigate)
            (define-key map (kbd "r") 'chrome-restart-stackframe)
            map))

(defun debugger-add-script-id (workerId scriptId url)
  ;; map in map
  (if (null chrome-script-id-map) ;; set global variable
      (setq chrome-script-id-map (make-hash-table :test 'equal))
      )
  (let* ((entry))
	 (setq entry (gethash workerId chrome-script-id-map))
	 (if (null entry)
	     (progn
	       (setq entry (make-hash-table :test 'equal))
	       (puthash workerId entry chrome-script-id-map)
	       )
	     ) 
	 ;; entry available
	 (puthash scriptId url entry)
	 ;; for debugging purposes
	 ;;(message (format "haha:%s" (gethash scriptId (gethash 0 chrome-script-id-map))))
	 )
)

(defun debugger-script-parsed (workerId params)
;; debugger script parsed - remember all script ids
  (let* ((scriptId (gethash "scriptId" params)) (url (gethash "url" params)) )
    (debugger-add-script-id workerId scriptId url)
    )
)

(defun debugger-worker-created (params)
  (let* ((inspectorConnected (gethash "inspectorConnected" params)) (workerId (gethash "workerId" params)))
	 (if (string-equal ":json-false" inspectorConnected)
	     (progn
	       ;; inspector not connected
	       (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.connectToWorker\",\"params\":{\"workerId\":%i}}" (chrome-next-id) workerId ))
	       )
	   )
	 ;;
	 (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":{\"id\":%i,\"method\":\"Console.enable\"}}}" (chrome-next-id) workerId (chrome-next-id)))
	 (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":{\"id\":%i,\"method\":\"Debugger.enable\"}}}" (chrome-next-id) workerId (chrome-next-id)))
	 (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":{\"id\":%i,\"method\":\"Runtime.enable\"}}}" (chrome-next-id) workerId (chrome-next-id)))
	 (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":{\"id\":%i,\"method\":\"Debugger.pause\"}}}" (chrome-next-id) workerId (chrome-next-id)))
	 ;; TODO: this is also needed for worker
	 ;; socket.session.getRemote().sendString(
	 ;; "{\"id\":" + nextId()
	 ;; +
	 ;; ",\"method\":\"Network.setCacheDisabled\",\"params\":{\"cacheDisabled\":true}}");
	 ;; socket.session.getRemote().sendString("{\"id\":"+nextId()+",\"method\":\"Network.enable\"}");//
	 ;; with this enabled, reload changed sources after reload page
	 ;; is working fine
	 (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":{\"id\":%i,\"method\":\"Runtime.run\"}}}" (chrome-next-id) workerId (chrome-next-id)))
	 (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":{\"id\":%i,\"method\":\"Debugger.setPauseOnExceptions\",\"params\":{\"state\":\"none\"}}}}" (chrome-next-id) workerId (chrome-next-id)))
	 (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":{\"id\":%i,\"method\":\"Debugger.setAsyncCallStackDepth\",\"params\":{\"maxDepth\":0}}}}" (chrome-next-id) workerId (chrome-next-id)))
	 )
)

(defun hide-arrow ()
  ;; remove marker
  (setq overlay-arrow-position nil)
  (remove-overlays)
)

(defun show-arrow ()
  ;;show marker 
  (setq overlay-arrow-position (make-marker))
  (set-marker overlay-arrow-position
	      (line-beginning-position ) )
  ;; overlay - mark line
  (if (null (boundp 'chrome-ov-map))
      (setq chrome-ov-map (make-hash-table :test 'equal))
    )
  (let* ((ov-by-buffer (gethash (current-buffer) chrome-ov-map)))
    (if (null ov-by-buffer)
	(progn
	  (setq ov-by-buffer (make-overlay (point-min) (point-min)))
	  (overlay-put ov-by-buffer 'face 'secondary-selection)
	  (puthash (current-buffer) ov-by-buffer chrome-ov-map)
	  )
      )
    (move-overlay ov-by-buffer (line-beginning-position) (line-beginning-position 2))   
    )
  )

(defun chrome-find-correct-url (scriptId functionName)
  (let ((map) (idx) (result))
    (setq idx (string-match "\\." functionName))
    (if (> idx 0)
	(setq functionName (substring functionName 0 idx))
      )
    (setq functionName (concat functionName "\\.js"))
    (setq map (gethash chrome-current-worker-id chrome-script-id-map))
    (maphash (lambda (kk vv) 
	       (progn
		(if (string-match functionName vv)
		    (setq result vv)
		  )
		)
	      ) map)
    ;; store in script map
    (puthash scriptId result map)
    result
    )
  )

(defun debugger-paused (params)
  (let* ( (callFrames (gethash "callFrames" params)) (firstStackEntry (elt callFrames 0)) (location) (lineNumber) (scriptId) (url))
    (setq location (gethash "location" firstStackEntry))
    (setq lineNumber (gethash "lineNumber" location))
    (setq scriptId (gethash "scriptId" location))
    ;; get file for script id - chrome-current-worker-id
    (setq url (gethash scriptId (gethash chrome-current-worker-id chrome-script-id-map)))
    ;;(message (format "scriptId is %s" scriptId))
    (if (null url) ;; url not found; happens after chrome-recompile-file
	(setq url (chrome-find-correct-url scriptId (gethash "functionName" firstStackEntry)))
	)
    (if (null chrome-local-root)
	(setq chrome-local-root (chrome-ensure-slash-as-last-character (read-directory-name "Local webcontent root folder:" (file-name-directory (buffer-file-name)))))
      )
  (if (null chrome-root-url)
      (setq chrome-root-url (chrome-ensure-slash-as-last-character (read-string "Web root url:" (car (get-hash-values (gethash 0 chrome-script-id-map))))))
    )
    ;; make relative path
    (setq url (concat chrome-local-root (substring url (length chrome-root-url) (length url))))
    ;; open file and mark line
    (find-file url)
    ;;(goto-line (1+ lineNumber))
    (goto-char (point-min))
    (forward-line lineNumber)
    (show-arrow)
    (setq chrome-current-callframes callFrames)
    )
  )

(defun chrome-get-runtime-properties (objectId gotoMax)
  ;; get object id  properties
  (let* ((json) (encodedObjectId))
    (setq encodedObjectId 
	  (with-temp-buffer
	    (insert objectId)
	    (goto-char (point-min))
	    (replace-string "\"" "\\\"")
	    (buffer-string)
	    )
	  )
    (setq chrome-runtime-properties-id (chrome-next-id))
    (setq chrome-runtime-properties-goto-max gotoMax)
    (setq json (format "{\"id\":%i,\"method\":\"Runtime.getProperties\",\"params\":{\"objectId\":\"%s\",\"ownProperties\":true,\"accessorPropertiesOnly\":false}}" chrome-runtime-properties-id encodedObjectId))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
)

(defun chrome-show-runtime-properties (values doGotoMax)
  (let* ((i (- (length values) 1) ) (result (make-hash-table :test 'equal)) (point-before) (point-after) (last) (first))
    (while (> i -1)
      (let* ((value (elt values i)) (name) (ty) (v nil) (hashvalue (gethash "value" value)) )
	(setq name (gethash "name" value))
	(setq ty (gethash "type" hashvalue))
	;; for debugging purposes
	;;(message ty)
	(if (string-equal ty "object")
	    (progn
	      (setq v (concat (gethash "description" hashvalue) "_@_" (gethash "objectId" hashvalue)))	
	      (setq v (with-temp-buffer
			(insert v)
			(goto-char (point-min))
			(replace-string "\"" "\\\"")
			(buffer-string)
			)
		    )
	      )
	  )
	(if (string-equal ty "number")
	    (setq v (gethash "value" hashvalue))
	    )
	(if (string-equal ty "string")
	    (setq v (gethash "value" hashvalue))
	    )
	(if (string-equal ty "boolean")
	    (setq v (gethash "value" hashvalue))
	    )
	(if (string-equal ty "function")
	    (setq v (gethash "description" hashvalue))
	    )
	(puthash name v result)
	(setq i (- i 1))
	)
      )
    (with-current-buffer (get-buffer-create "*Chrome Evaluation Result*")
      (js-mode)
      (hs-minor-mode) ;; for collapse and expand
      ;;TODO: c-collapse; e-expand shortcuts?
      (if doGotoMax
	  (goto-char (point-max))
	;; else - replace value
	(progn
	  (search-backward "\": \"")
	  (goto-char (+ (point) 3))
	  (kill-line)
	  (insert ",")
	  (goto-char (- (point) 1))
	  ;;(replace-regexp ".*open\"" "")
	  )
	)
      (if chrome-current-eval-object-classname
	  (progn
	    (insert "\n")
	    (insert chrome-current-eval-object-classname)
	    (setq chrome-current-eval-object-classname nil)
	    )
	  )
      (setq point-before (point))
      (insert (json-encode result))
      (setq point-after (point))
      (json-reformat-region point-before point-after)
      (if (null doGotoMax)
	  (indent-region point-before point-after)
	  )
      (setq last point-after)
      (setq first point-before)
      (if doGotoMax
	  (insert "\n")
	)
      ;; add link for all {"injectedScriptId....
      (catch 'break 
	(while t
	  (setq point-before (search-forward "@_{" nil t))
	  (if (null point-before)
	      (throw 'break nil) 
	    )
	  (if (> point-before last)
	      (progn
		(search-backward "}open")
		(throw 'break nil) 
		)
	      )
	  (setq point-before (- point-before  1))
	  (setq point-after (+ (search-forward "}") 0))
	  (insert-button "open"
			 'action (lambda (x) (progn
					       (goto-char (button-get x 'first))
					       (search-forward (button-get x 'url))
					       (let* ((url (button-get x 'url) ))
						 (setq url (with-temp-buffer
							   (insert url)
							   (goto-char (point-min))
							   (replace-string "\\\"" "\"")
							   (buffer-string)
							   )
						       )
						 (chrome-get-runtime-properties url nil)
						 )
					      )
				   )
			 'url (buffer-substring point-before point-after)
			 'first first)
	  )
	)
;;      (split-window-horizontally)
      (if doGotoMax
	  (progn
	    (switch-to-buffer-other-window (current-buffer))
	    (end-of-buffer) ;; jump to end
	    (other-window 1)
	    )
	)
      )
    )
  )

(defun chrome-show-simple-value (value)
   (with-current-buffer (get-buffer-create "*Chrome Evaluation Result*")
      (js-mode)
      (goto-char (point-max))
      (setq point-before (point))
      (insert (json-encode value))
      (setq point-after (point))
      (json-reformat-region point-before point-after)
      (insert "\n")
      (switch-to-buffer-other-window (current-buffer))
      (end-of-buffer) ;; jump to end
      (other-window 1)
      )
)

(defun debugger-dispatch-from-worker (params)
  (let* ((workerId (gethash "workerId" params)) (message (gethash "message" params)) (method) )
    (catch 'break 
      (setq method (gethash "method" message))
      (if (null method)
	  (progn
	    (if (equal (gethash "id" message) chrome-eval-expr-id)  ;; TODO: extract duplicate (gethash "id" msg) calls
		(progn
		  (let* ((res) (objid))
		    ;;(message payload)
		    (setq res (gethash "result" (gethash "result" message)))
		    (setq objid (gethash "objectId" res))
		    (setq chrome-current-eval-object-classname (gethash "className" res))
		    (if (null objid)
			(chrome-show-simple-value res)
		      ;; else
		      (chrome-get-runtime-properties objid t)
		      )
		    )
		  (throw 'break nil) 
		  )
	      )
	    (if (equal (gethash "id" message) chrome-runtime-properties-id)
		(progn
		  ;;(message payload)
		  (chrome-show-runtime-properties (gethash "result" (gethash "result" message)) chrome-runtime-properties-goto-max)
		  (throw 'break nil) 
		  )
	      )
	    (if (equal (gethash "id" message) chrome-restart-frame-id)
	       (progn
		 (other-window 1)
		 (chrome-step-into)
		 (throw 'break nil) 
		 )
	     )
	    (throw 'break nil) 
	    )
	)
      (if (string-equal method "Debugger.scriptParsed")
      	  (progn
      	    (debugger-script-parsed workerId (gethash "params" message))
      	    (throw 'break nil) 
      	    )
      	)
     (if (string-equal method "Debugger.paused")
	 (progn
	   (setq chrome-current-worker-id workerId) ;; set current global worker id
	   (debugger-paused (gethash "params" message))
	   (throw 'break nil) 
	   )
       )
     (if (string-equal method "Debugger.resumed")
	 (progn
	   (setq chrome-current-worker-id 0) ;; reset  global worker id
	   (throw 'break nil) 
	   )
	 )
      (message (format "not handeled from worker:%s" (prin1-to-string message)))
      )
    )
)

(defun chrome-on-message (websocket frame)
 (let* ((json-object-type 'hash-table) (payload (websocket-frame-payload frame)) (msg (with-temp-buffer
                   (insert payload)
                   (goto-char (point-min))
                   (json-read))) (method) )
   (catch 'break 
     (setq method (gethash "method" msg))
     (if (null method)
	 (progn
	   (if (null (gethash "id" msg))
	       (progn 
		 ;;TODO: don't log when msg is empty
		 (message payload)
		 (throw 'break nil)
		 )
	     )
	   (if (equal (gethash "id" msg) chrome-eval-expr-id)  ;; TODO: extract duplicate (gethash "id" msg) calls
	       (progn
		 (let* ((res) (objid))
		   ;;(message payload)
		   (setq res (gethash "result" (gethash "result" msg)))
		   (setq objid (gethash "objectId" res))
		   (setq chrome-current-eval-object-classname (gethash "className" res))
		   (if (null objid)
		       (chrome-show-simple-value res)
		     ;; else
		     (chrome-get-runtime-properties objid t)
		     )
		   )
		 (throw 'break nil) 
		 )
	     )
	   (if (equal (gethash "id" msg) chrome-runtime-properties-id)
	       (progn
		 ;;(message payload)
		 (chrome-show-runtime-properties (gethash "result" (gethash "result" msg)) chrome-runtime-properties-goto-max)
		 (throw 'break nil) 
		 )
	     )
	   (if (equal (gethash "id" msg) chrome-restart-frame-id)
	       (progn
		 (other-window 1)
		 (chrome-step-into)
		 (throw 'break nil) 
		 )
	     )
	   (if (equal (gethash "id" msg) chrome-complete-id)
	       (progn
		 (chrome-call-function-on-to-get-completions (gethash "result" msg))
		 (throw 'break nil) 
		 )
	     )
	   (if (equal (gethash "id" msg) chrome-call-function-id)
	       (progn
		 (chrome-fill-completion-results-and-release-group (gethash "result" msg))
		 (throw 'break nil) 
		 )
	     )
	   (if (equal (gethash "id" msg) chrome-recompile-id)
	       (progn
		 ;;(message payload)
		 (chrome-analyze-recompile-result msg)
		 (throw 'break nil) 
		 )
	       )
	   (let ((re (gethash "result" msg)))
	     (if re
		 (if (= (hash-table-count re) 0)
		     (throw 'break nil)
		     )
		 )
	     )
	   (message payload)
	   (throw 'break nil) 
	   )
       )
     (if (string-equal method "Console.messageAdded")
	 (progn 
	   (add-console-message (gethash "params" msg))
	   (throw 'break nil) 
	   )
       )
     (if (string-equal method "Debugger.scriptParsed")
	 (progn
	   (debugger-script-parsed 0 (gethash "params" msg))
	   (throw 'break nil) 
	 )
       )
     (if (string-equal method "Worker.workerCreated")
	 (progn
	   (debugger-worker-created (gethash "params" msg))
	   (throw 'break nil) 
	   )
	 )
     (if (string-equal method "Worker.dispatchMessageFromWorker")
	 (progn
	   (debugger-dispatch-from-worker (gethash "params" msg))
	   (throw 'break nil) 
	   )
       )
     (if (string-equal method "Debugger.paused")
	 (progn
	   (setq chrome-current-worker-id 0) ;; set current global worker id
	   (debugger-paused (gethash "params" msg))
	   (throw 'break nil) 
	   )
	 )
     ;; TODO: how to make "or" expression in elisp
     (if (string-equal method "Debugger.resumed")
	 (progn
	   (throw 'break nil) 
	   )
	 )
     (if (string-equal method "Network.loadingFinished")
	 (progn
	   (throw 'break nil) 
	   )
	 )
     (if (string-equal method "Network.requestWillBeSent")
	 (progn
	   (throw 'break nil) 
	   )
	 )
     (if (string-equal method "Network.dataReceived")
	 (progn
	   (throw 'break nil) 
	   )
	 )
     (if (string-equal method "Network.responseReceived")
	 (progn
	   (throw 'break nil) 
	   )
	 )
     (if (string-equal method "Network.requestServedFromCache")
	 (progn
	   (throw 'break nil) 
	   )
	 )	 
     ;; message only the stuff which I don't understand yet
     (message payload)
     )
   ;; only for debugging purposes
;;   (message payload)
   ;;(message (prin1-to-string msg))
   )
;; TODO: how to handle breakpointIds -> do we have to remember them?
)

(defun chrome-on-open (websocket)
  (message "onopen")
)

(defun chrome-on-close (websocket)
  (message "onclose")
)

(defun chrome-on-error (websocket action error)
  (message (format "onerror:%s;%s" action error))   
)

(defun send-ws-message (msg)
   (websocket-send-text current-ws msg)
   )

(defun chrome-ensure-slash-as-last-character (string)
  (let ((last))
    (setq last (elt string (- (length string) 1)))
    (if (not (char-equal last ?/))
	(setq string (concat string "/"))
      )
    )
  string
  )

;; test
;;(chrome-ensure-slash-as-last-character "c:/test/test1/")

(defun chrome-map-to-filename (url)
    (if (null chrome-local-root)
      (setq chrome-local-root (chrome-ensure-slash-as-last-character (read-directory-name "Local webcontent root folder:"  (file-name-directory (buffer-file-name)))))
    )
  (if (null chrome-root-url)
      (setq chrome-root-url (chrome-ensure-slash-as-last-character (read-string "Web root url:" (car (get-hash-values (gethash 0 chrome-script-id-map))))))
    )
  (let* ((rest))
    (setq rest (substring url (length chrome-root-url) (length url)))
    (concat chrome-local-root rest)
    )
)

(defun chrome-map-to-url (filename)
  (if (null chrome-local-root)
      (setq chrome-local-root (chrome-ensure-slash-as-last-character (read-directory-name "Local webcontent root folder:" (file-name-directory filename))))
    )
  (if (null chrome-root-url)
      (setq chrome-root-url (chrome-ensure-slash-as-last-character (read-string "Web root url:" (car (get-hash-values (gethash 0 chrome-script-id-map))))))
    )
  (let* ((rest))
    (setq rest (substring filename (length chrome-local-root) (length filename)))
    (concat chrome-root-url rest)
    )
)

(defun get-hash-values (hashtable)
  "Return all values in HASHTABLE."
  (let (allvals)
    (maphash (lambda (kk vv) (setq allvals (cons vv allvals))) hashtable)
    allvals
  )
)

(defun get-hash-keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys
  )
)

(defun get-hash-key-for-value (hashtable given-value)
  (let ((result nil))
   (maphash (lambda (kk vv) 
	      (progn
		(if (string-equal vv given-value)
		    (setq result kk)
		    )
		)
	      ) hashtable)
   result
   )
)

(defun has-hash-value (hashtable given-value)
  (let ((result nil))
   (maphash (lambda (kk vv) 
	      (progn
		(if (string-equal vv given-value)
		    (setq result t)
		    )
		)
	      ) hashtable)
   result
   )
)

(defun has-hash-key (hashtable given-value)
  (let ((result nil))
   (maphash (lambda (kk vv) 
	      (progn
		(if (string-equal kk given-value)
		    (setq result t)
		    )
		)
	      ) hashtable)
   result
   )
)

;; for debugging
;;(setq chrome-root-url nil)

(defun chrome-step-over ()
  (interactive)
  (let* ((json))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.stepOver\"}" (chrome-next-id)))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
)

(defun chrome-step-into ()
  (interactive)
    (let* ((json))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.stepInto\"}" (chrome-next-id)))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
)

(defun chrome-step-out ()
  (interactive)
  (let* ((json))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.stepOut\"}" (chrome-next-id)))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
)

(defun chrome-continue ()
  (interactive)
  (hide-arrow)
  (let* ((json))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.resume\"}" (chrome-next-id)))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
)

(defun chrome-analyze-recompile-result (result)
  (let* ((err))
    (setq err (gethash "error" result))
    (if (null err)
	;; aha no error
	(progn
	  (setq err (gethash "result" result))
	  (setq err (json-encode err))
	  (message err)
	  )
      ;;else -> error case
      (progn
	(setq err (gethash "message" err))
	(message err)
	)
      )
    )
  )

(defun chrome-recompile-file ()
  (interactive)
;;TODO send also to workers ...
;; TODO: test worker debugging
;;  - chrome-recompile-file not working inside worker
  (let* ((json) (source) (scripts) (scriptId) (url (chrome-map-to-url (buffer-file-name))))
    (setq source (strip-text-properties (buffer-string)))
    (setq source 
	  (with-temp-buffer
	    (insert source)
	    (goto-char (point-min))
	    (replace-string "\\"  "\\\\")
	    (goto-char (point-min))
	    (replace-string "\"" "\\\"")
	    (goto-char (point-min))
	    (replace-string "\n" "\r\n") ;; TODO: question: is this only windows specific?
	    (buffer-string)
	  ))
    (setq scripts (gethash chrome-current-worker-id chrome-script-id-map))    
    (setq scriptId (get-hash-key-for-value scripts url))
    ;;    (has-hash-value scripts url)
    (setq chrome-recompile-id (chrome-next-id))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.setScriptSource\",\"params\":{\"scriptId\":\"%s\",\"scriptSource\":\"%s\"}}" chrome-recompile-id scriptId source) )
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
  )

(defun chr-get-current-line ()
       "returns the current line number (in the buffer) of point."
       (interactive)
       (save-restriction
         (widen)
         (save-excursion
           (beginning-of-line)
	   (1+ (count-lines 1 (point)))))
       )

(defun chrome-restart-stackframe ()
  (interactive)
  (let ( (idx) (callFrame) (callFrameId) (json) )
    (setq idx (- (chr-get-current-line) 3))
    (setq callFrame (elt chrome-current-callframes idx))
    (setq callFrameId (gethash "callFrameId" callFrame))
    (setq callFrameId 
	  (with-temp-buffer
	    (insert callFrameId)
	    (goto-char (point-min))
	    (replace-string "\"" "\\\"")
	    (buffer-string)
	    )
	  )
    (setq chrome-restart-frame-id (chrome-next-id))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.restartFrame\",\"params\":{\"callFrameId\":\"%s\"}}" chrome-restart-frame-id callFrameId ))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
  )

(defun chrome-show-console ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Chrome Console*")
    (switch-to-buffer-other-window (current-buffer))
      )
  )

(defun chrome-show-stack () 
  (interactive)
  (with-current-buffer (get-buffer-create "*Chrome Call Stack*")
    (if (not (bound-and-true-p chrome-console-nav-mode))
	(chrome-console-nav-mode) ;; enable nav mode
      )
    (erase-buffer)
    (insert "Chrome Call Stack\n")
    (insert "=================\n")
    (let* ((callFrameLength (length chrome-current-callframes)) (i 0) (frame) (text) (locattion) (lineNumber) (columnNumber) (scriptId) (url))
      (while (< i callFrameLength)
	(setq frame (elt chrome-current-callframes i))
	(setq text (gethash "functionName" frame))
	(setq location (gethash "location" frame))
	(setq lineNumber (gethash "lineNumber" location))
	(setq columnNumber (gethash "columnNumber" location))
	(setq scriptId (gethash "scriptId" location))
	(insert text)
	(insert " ")
	(setq url (gethash scriptId (gethash chrome-current-worker-id chrome-script-id-map)))
	(setq lineNumber (1+ lineNumber))
	(setq columnNumber (1+ columnNumber))
	(insert (format "(%s:%i:%i)" url lineNumber columnNumber)) ;; linenumber columnnumber
	(insert "\n")
	(setq i (1+ i))
	)
      )
    (switch-to-buffer-other-window (current-buffer))
    )
)

(defun chrome-reload-page ()
  (interactive)
  (let* ((json))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.setSkipAllPauses\",\"params\":{\"skipped\":true,\"untilReload\":true}}" (chrome-next-id)))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    ;;
    (setq json (format "{\"id\":%i,\"method\":\"Page.reload\",\"params\":{\"ignoreCache\":true}}" (chrome-next-id)))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
  )

(defun chrome-disable-all-breakpoints ()
  (interactive)
  (let* ((json))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.setBreakpointsActive\",\"params\":{\"active\":false}}" (chrome-next-id) ))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
  )

(defun chrome-enable-all-breakpoints ()
  (interactive)
  (let* ((json))
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.setBreakpointsActive\",\"params\":{\"active\":true}}" (chrome-next-id) ))
    (if (not (equal chrome-current-worker-id 0))
	;; web worker
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
      )
    (send-ws-message json)
    )
)

;; completion process looks like this
;;send:{"id":135,"method":"Debugger.evaluateOnCallFrame","params":{"callFrameId":"{\"ordinal\":0,\"injectedScriptId\":2}","expression":"this","objectGroup":"completion","includeCommandLineAPI":true,"doNotPauseOnExceptionsAndMuteConsole":true,"returnByValue":false,"generatePreview":false}}

;;send:{"id":136,"method":"Runtime.callFunctionOn","params":{"objectId":"{\"injectedScriptId\":2,\"id\":5380}","functionDeclaration":"function getCompletions(primitiveType)\r\n{var object;if(primitiveType===\"string\")\r\nobject=new String(\"\");else if(primitiveType===\"number\")\r\nobject=new Number(0);else if(primitiveType===\"boolean\")\r\nobject=new Boolean(false);else\r\nobject=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i)\r\nresultSet[names[i]]=true;}catch(e){}}\r\nreturn resultSet;}","doNotPauseOnExceptionsAndMuteConsole":true,"returnByValue":true,"generatePreview":false}}

;;send:{"id":137,"method":"Runtime.releaseObjectGroup","params":{"objectGroup":"completion"}}

;; 3rd complete step
(defun chrome-fill-completion-results-and-release-group (result)
  (let* ((compl (gethash "value" (gethash "result" result)) )  (compls) ) 
    (setq compls (get-hash-keys compl))
    (setq chrome-completions compls) ;; set global variable
  )
  ;; release
  (send-ws-message (format "{\"id\":%i,\"method\":\"Runtime.releaseObjectGroup\",\"params\":{\"objectGroup\":\"completion\"}}" (chrome-next-id)))
)

;; 2nd complete step
(defun chrome-call-function-on-to-get-completions (result)
  (let* ((objectid (gethash "objectId" (gethash "result" result)) ) (json)) 
    (setq chrome-call-function-id (chrome-next-id))
    (setq objectid
	  (with-temp-buffer
	    (insert objectid)
	    (goto-char (point-min))
	    (replace-string "\"" "\\\"")
	    (buffer-string)
	    )
	  )
    (setq json (format "{\"id\":%i,\"method\":\"Runtime.callFunctionOn\",\"params\":{\"objectId\":\"%s\",\"functionDeclaration\":\"function getCompletions(primitiveType) {var object;if(primitiveType===\\\"string\\\")object=new String(\\\"\\\");else if(primitiveType===\\\"number\\\")object=new Number(0);else if(primitiveType===\\\"boolean\\\")object=new Boolean(false);else object=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i) resultSet[names[i]]=true;}catch(e){}} return resultSet;}\",\"doNotPauseOnExceptionsAndMuteConsole\":true,\"returnByValue\":true,\"generatePreview\":false}}" chrome-call-function-id objectid))
    (send-ws-message json)
  )
)

;; 1st complete step
(defun chrome-complete (exprParam)
  (interactive)
  ;; TODO: worker
  (let* ((json) (expr) (contextId) (firstCallFrame (elt chrome-current-callframes 0)) )
    (setq callFrameId (gethash "callFrameId" firstCallFrame))
    (setq callFrameId 
	  (with-temp-buffer
	    (insert callFrameId)
	    (goto-char (point-min))
	    (replace-string "\"" "\\\"")
	    (buffer-string)
	    )
	  )
    (setq expr exprParam)
    (setq chrome-complete-id (chrome-next-id))
    (setq chrome-completions nil) ;; reset result
    (setq json (format "{\"id\":%i,\"method\":\"Debugger.evaluateOnCallFrame\",\"params\":{\"callFrameId\":\"%s\",\"expression\":\"%s\",\"objectGroup\":\"completion\",\"includeCommandLineAPI\":true,\"doNotPauseOnExceptionsAndMuteConsole\":true,\"returnByValue\":false,\"generatePreview\":false}}" chrome-complete-id callFrameId expr))
    (send-ws-message json)
    ;; wait for the completion result
    (while (null chrome-completions)
      (sit-for 1)
      )
    chrome-completions
    )
)

(defun chrome-js-is-ws (ch)
  (if (or (char-equal ch ?\s) 
	  (char-equal ch ?\t) 
	  (char-equal ch ?\n) 
	  (char-equal ch ?\r)) 
      t)
)

(defun chrome-get-js-expression-at-point ()
  (interactive)
  (let* ((c) (start-point) (end-point) (bracket-count) (str))
    (with-current-buffer (current-buffer)
      (setq start-point (point))
      (setq bracket-count 0)
      ;; go to right
      (catch 'break 
	(while t
	  (setq c (char-after (point)))
	  (if (and (char-equal c ?=) (<= bracket-count 0))
		(throw 'break nil)
	      )
	  (if (char-equal c ?\()
	      (setq bracket-count (1+ bracket-count))
	      )
	  (if (char-equal c ?\))
	      (setq bracket-count (- bracket-count 1))
	      )
	  (if (and (char-equal c ?\)) (<= bracket-count 0))
	      (progn
		(if (= bracket-count 0)
		    (goto-char (1+ (point)))
		  )
		(throw 'break nil)
		)
	    )
	  (if (and (chrome-js-is-ws c) (<= bracket-count 0) ) ;; stop at whitespace
	      (throw 'break nil)
	      )
	  (if (and (char-equal c ?,) (<= bracket-count 0) ) ;; stop at comma
	      (throw 'break nil)
	    )
	  (if (and (char-equal c ?+) (<= bracket-count 0) ) ;; stop at +
	      (throw 'break nil)
	      )
	  (if (and (char-equal c ?-) (<= bracket-count 0) ) ;; stop at -
	      (throw 'break nil)
	      )
	  (if (char-equal c ?\;) ;; stop at ";"
	      (progn
		(throw 'break nil)
		)
	    )
	  (goto-char (1+ (point)))
	  (if (equal (point-max) (point))
	      (throw 'break nil)
	      )
	  )
	)
      (setq end-point (point))
      (goto-char start-point)
      (setq bracket-count 0)
      ;; go to left 
      (catch 'break 
	(while t
	  (setq c (char-after (point)))
	  (if (and (= 0 bracket-count) (char-equal c ?\())
	      (progn
		(goto-char (1+ (point)))
		(throw 'break nil)
		)
	      )
	  (if (char-equal c ?\))
	      (setq bracket-count (1+ bracket-count))
	      )
	  (if (char-equal c ?\()
	      (setq bracket-count (- bracket-count 1))
	      )
	  (if (chrome-js-is-ws c)
	      (throw 'break nil)
	      )
	  (if (char-equal c ?=)
	      (progn
		(goto-char (1+ (point)))
		(throw 'break nil)
		)
	    )
	  (if (and (char-equal c ?,) (<= bracket-count 0) ) ;; stop at ,
	      (progn
		(goto-char (1+ (point)))
		(throw 'break nil)
		)
	    )
	  (if (and (char-equal c ?+) (<= bracket-count 0) ) ;; stop at +
	      (progn
		(goto-char (1+ (point)))
		(throw 'break nil)
		)
	    )
	  (goto-char (- (point) 1))
	  )
	)
	;; js encode
	(setq str (buffer-substring (point) end-point))
	(with-temp-buffer
	    (insert str)
	    (goto-char (point-min))
	    (replace-string "\"" "\\\"")
	    (buffer-string)
	    )
      )
  )
)

(defun chromeCallBackComplete (string pred mode)
  (setq string (chrome-trim-string (strip-text-properties string)))
  (cond ((not mode)
	 ;; if unique result, then return complete result
	 (let ((result string) (matches))
	   (if chromeLastCompleteList
	       (progn
		 (if (< (length string) (length chromeLastCompleteString))
		     (progn
		       (setq chromeLastCompleteString nil)
		       (setq chromeLastCompleteList nil)
		       )
		     ;;else
		     (progn
		       (setq matches (all-completions string chromeLastCompleteList))
		       (if (= (length matches) 1)
			   (setq result (car matches))
			 )
		       )
		   )
	       )
	     )
	   result
	   )
	 )
	((eq mode 't) 
	 (let ((sub) (rest) (list) (result-list '()) (compl))
	   (setq sub (with-temp-buffer
		       (insert string)
		       (goto-char (point-max))
		       (let* ((idx (search-backward "." nil t) )) 
			 (if idx
			     (progn
			       (setq res (buffer-substring idx (point-max)))
			       (kill-rectangle idx (point-max))
			       )
			   )
			 )
		       (strip-text-properties (buffer-string))
		       )
		 )
	   (setq sub (chrome-trim-string sub))
	   (message sub)
	   (if (> (length sub) 0)
	       (progn 
		 (setq list (chrome-complete sub))
		 (while list
		   (setq compl (concat sub "." (car list)))
		   (if (> (length res) 0) ;; filter the result
		       (progn
			 (if (string-prefix-p string compl t)   
			     (setq result-list (cons compl result-list))
			     )
			 )
			 ;;else
		     (setq result-list (cons compl result-list))
		     )
		   (setq list (cdr list))
		   )
		 (setq chromeLastCompleteList result-list)
		 (setq chromeLastCompleteString string)
		 result-list
	       )
	   )
	   )
	 ) 
	((eq mode 'lambda)
					;(list "aa"  "a0bbb")
	 )
					;('t (cons (list "aaa" "aab" "bbb") 1) )
	)
  )

(defun chrome-trim-string (str)
      "Chomp leading and tailing whitespace from STR."
      (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                        (: (* (any " \t\n")) eos)))
                                ""
                                str))


(defun chrome-evaluate-expression ()
  (interactive)
  ;; TODO: eval expression when in lower stack entries
  (let* ((firstCallFrame (elt chrome-current-callframes 0)) (callFrameId) (json) (expr) (id)) 
    (setq chromeLastCompleteList nil)
    (setq expr (completing-read "Eval:" 'chromeCallBackComplete nil nil (chrome-get-js-expression-at-point)))
    (setq callFrameId (gethash "callFrameId" firstCallFrame))
    (setq callFrameId 
	  (with-temp-buffer
	    (insert callFrameId)
	    (goto-char (point-min))
	    (replace-string "\"" "\\\"")
	    (buffer-string)
	    )
	  )
    (setq id (chrome-next-id))
    (setq chrome-eval-expr-id id)
    (setq  json (format "{\"id\":%i,\"method\":\"Debugger.evaluateOnCallFrame\",\"params\":{\"callFrameId\":\"%s\",\"expression\":\"%s\",\"objectGroup\":\"console\",\"includeCommandLineAPI\":true,\"doNotPauseOnExceptionsAndMuteConsole\":false,\"returnByValue\":false,\"generatePreview\":false}}" id callFrameId expr))
    (if (not (equal chrome-current-worker-id 0))
	(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) chrome-current-worker-id json))
	)
    (send-ws-message json)
    )
)

(defun chrome-remove-breakpoint ()
  (interactive)
  ;; url looks like http://localhost:5040/test.js:64:0
  (let* ( (url (chrome-map-to-url (buffer-file-name))) (lineNumber) (workerNos))
    (setq lineNumber (count-lines 1 (point)))
    (setq workerNos (get-hash-keys chrome-script-id-map))
    (while workerNos ;; loop over all workers
      (let ((wno (car workerNos)) (json nil) (urlWithNum nil) )
	(setq scripts (gethash wno chrome-script-id-map))
	(if (has-hash-value scripts url) ;; is file loaded
	    (if (= wno 0)
		(progn
		  (setq urlWithNum (format "%s:%i:0" url lineNumber))
		  (setq json (format "{\"id\":%i,\"method\":\"Debugger.removeBreakpoint\",\"params\":{\"breakpointId\":\"%s\"}}" (chrome-next-id) urlWithNum))
;;		  (message json)
		  )
	      ;;else -> not worker 0
	      (progn
		(setq urlWithNum (format "%s:%i:0" url lineNumber))
		(setq json (format "{\"id\":%i,\"method\":\"Debugger.removeBreakpoint\",\"params\":{\"breakpointId\":\"%s\"}}" (chrome-next-id) urlWithNum))
		(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) wno json))
		)
	      )
	  )
	(if json
	    (send-ws-message json)
	  )
	(setq workerNos (cdr workerNos)))
      )
    )
  )

(defun chrome-add-breakpoint ()
  (interactive)
  ;; map to url
  (let* ( (url (chrome-map-to-url (buffer-file-name))) (json) (lineNumber) (workerNos) (scripts) )
    (setq lineNumber (count-lines 1 (point)))
    (setq workerNos (get-hash-keys chrome-script-id-map))
    (while workerNos ;; loop over all workers
      (let ((wno (car workerNos)) (json nil) )
	(setq scripts (gethash wno chrome-script-id-map))
	(if (has-hash-value scripts url) ;; is file loaded
	    (if (= wno 0)
		(progn
		  (setq json (format "{\"id\":%i,\"method\":\"Debugger.setBreakpointByUrl\",\"params\":{\"lineNumber\":%i,\"url\":\"%s\",\"columnNumber\":0,\"condition\":\"\"}}" (chrome-next-id) lineNumber url))
;;		  (message json)
		  )
	      ;;else -> not worker 0
	      (progn
		(setq json (format "{\"id\":%i,\"method\":\"Debugger.setBreakpointByUrl\",\"params\":{\"lineNumber\":%i,\"url\":\"%s\",\"columnNumber\":0,\"condition\":\"\"}}" (chrome-next-id) lineNumber url))
		(setq json (format "{\"id\":%i,\"method\":\"Worker.sendMessageToWorker\",\"params\":{\"workerId\":%i,\"message\":%s}}" (chrome-next-id) wno json))
		)
	      )
	  )
	(if json
	    (send-ws-message json)
	    )
	(setq workerNos (cdr workerNos)))
	)
      )
  )

(defun chrome-disconnect ()
  (interactive)
  (if (boundp 'curret-ws) 
      (websocket-close current-ws)
    )
  (clear-all-chrome-variables)
)

(defun clear-all-chrome-variables ()
  (setq current-ws nil)
  (setq chrome-script-id-map nil)
  (setq chrome-root-url nil)  ;; web url root path
  (setq chrome-local-root nil) ;; local root url path
  (setq chrome-current-worker-id nil)
)

(defun chrome-connect ()
  (interactive)
  (chrome-disconnect)
  (let* ((wsurl   (get-websocket-url-from-chrome)))
    (message (concat "Connect to : " wsurl))
    (if (> (length wsurl) 0)
	(progn
	  (setq current-ws (websocket-open wsurl
					   :on-message (lambda (websocket frame)
							 (chrome-on-message websocket frame))
					   :on-open (lambda (websocket)
						      (chrome-on-open websocket))
					   :on-close (lambda (websocket)
						       (chrome-on-close websocket))
					   :on-error (lambda (websocket action error)
						       (chrome-on-error websocket action error)
						       )
					   )
		)
	  (setq current-id 1)
	  (setq chrome-script-id-map nil)
	  (setq chrome-root-url nil)  ;; web url root path
	  (setq chrome-local-root nil) ;; local root url path
	  (setq chrome-eval-expr-id nil)
	  (setq chrome-runtime-properties-id nil)
	  (setq chrome-recompile-id nil)
	  (setq chrome-current-eval-object-classname nil)
	  (setq chrome-complete-id nil)
	  (setq chrome-call-function-id nil)
	  (setq chrome-current-worker-id 0)
	  (setq chrome-restart-frame-id nil)
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.canInspectWorkers\"}" (chrome-next-id)))
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Console.enable\"}" (chrome-next-id)))
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Network.setCacheDisabled\",\"params\":{\"cacheDisabled\":true}}" (chrome-next-id)))
	  ;; with this enabled, reload changed sources after reload page is working fine
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Network.enable\"}" (chrome-next-id)))
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Debugger.enable\"}" (chrome-next-id)))
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.enable\"}" (chrome-next-id)))
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Debugger.setPauseOnExceptions\",\"params\":{\"state\":\"none\"}}" (chrome-next-id)))
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Debugger.setAsyncCallStackDepth\",\"params\":{\"maxDepth\":0}}" (chrome-next-id)))
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Debugger.skipStackFrames\",\"params\":{\"script\":\"\"}}" (chrome-next-id)))
	  (send-ws-message (format "{\"id\":%i,\"method\":\"Worker.setAutoconnectToWorkers\",\"params\":{\"value\":true}}" (chrome-next-id)))
	  )
      ;; else
      (message "No WS Url found")
      )
    )
  )

(defun chrome-next-id ()
  (setq current-id (1+ current-id))
  current-id
)

;;;###autoload

(provide 'chrome_debugger)

;;; chrome_debugger.el ends here

