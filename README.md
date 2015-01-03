Chrome Javascript Debugger for Emacs
====================================

Javascript debugger using the chrome debug websocket protocol (https://developer.chrome.com/devtools/docs/debugger-protocol).
Debug your Javascript code directly in emacs. All the known debugger features
from the chrome developer tools like stepping, show stack, restart frame,
object evaluation, file recompilation, debugging of web workers are supported. 

Chrome needs to be started with following command line parameter so that the debug port is open:
`chrome --remote-debugging-port=9222`

## Following interactive emacs commands are provided:
- chrome-connect                 - connect to running chrome instance
			           (use tab completion to find the tab to be debugged)  
- chrome-disconnect              - disconnect from current debug session
- chrome-add-breakpoint          - add a breakpoint
- chrome-remove-breakpoint       - remove a breakpoint
- chrome-step-into               - step into 
- chrome-step-over               - step over
- chrome-step-out                - step out 
- chrome-continue                - continue
- chrome-evaluate-expression     - evaluate expression at current point (completion by pressing tab)
- chrome-show-stack              - show stack list when in debugging mode
    commands inside show stack mode:
    - chrome-restart-stackframe - re-run stackframe at current point
    - chrome-console-navigate   - navigate to source for stackframe at current point
- chrome-show-console            - show console
- chrome-recompile-file          - recompiles current file and uploads to running chrome debug session
- chrome-disable-all-breakpoints - disable all breakpoints
- chrome-enable-all-breakpoints  - enables all breakpoints

## Dependencies:
- websocket
- json-reformat

both packages can be installed via M-x package-install 

## Sample .emacs config to register debugger shortcuts: 

   (add-to-list 'load-path "<path to chrome_debugger.el>")
   (defun my_javascript_keys ()
   	  (require 'chrome_debugger)
	  (local-set-key (kbd "<f5>") 'chrome-step-into)
	  (local-set-key (kbd "<f6>") 'chrome-step-over)
	  (local-set-key (kbd "<f7>") 'chrome-step-out)  
	  (local-set-key (kbd "<f9>") 'chrome-continue)  
	  (local-set-key (kbd "<f10>") 'chrome-evaluate-expression)
	  )
    ;;(add-hook 'js-mode-hook 'my_javascript_keys)
    (add-hook 'js2-mode-hook 'my_javascript_keys)

## Status:
alpha
