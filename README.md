chrome_debugger
===============

Chrome Javascript Debugger for Emacs

Javascript debugger using the chrome debug websocket protocol. Debug
your Javascript code directly in emacs. All the known debugger features
from the chrome developer tools like stepping, show stack, restart frame,
object evaluation, file recompilation, debugging of web workers are supported. 

chrome needs to be started with following command line parameter
 	chrome --remote-debugging-port=9222
so that the debug port is open.

Following interactive emacs commands are
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

Dependencies:
- websocket
- json-reformat
both packages can be installed via M-x package-install 

Status:
alpha
