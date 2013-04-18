(require 'websocket) ;; package-install websocket
(require 'dash)
(require 'json)

(defun extract-title-and-ws-address-pair (el)
  (let ((title (cdr (assoc 'title el)))
        (ws-address (cdr (assoc 'webSocketDebuggerUrl el))))
    (list title ws-address)))

(defun get-available-tabs ()
  (let* ((response (json-read-from-string (shell-command-to-string "curl -s http://localhost:9222/json"))))
    (-map 'extract-title-and-ws-address-pair response)))

(-filter (lambda (el) (string= "Google" (car el))) (get-available-tabs))









(setq ws (websocket-open
          "ws://localhost:9222/devtools/page/12"
          :on-open (lambda (websocket)
                     (message "Websocket opened"))
          :on-message (lambda (websocket frame)
                        (message "ws frame: %S" (assoc 'result (assoc 'result (json-read-from-string (websocket-frame-payload frame))))))
          :on-close (lambda (websocket)
                      (message "Websocket closed"))))



(websocket-send-text ws (json-encode '(:id 12 :method "Page.reload")))

(defun eval-in-browser (expr)
  "Evaluate expresion in the web browser's console"
  (websocket-send-text ws (json-encode `(:id 12 :method "Runtime.evaluate" :params (:expression ,expr)))))


(eval-in-browser "function add(a,b) { return a+b; }; add(2, 10);")

(eval-in-browser "MM.categoryListView.children[0].model.get('name')")
