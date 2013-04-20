;; package-install websocket
(require 'websocket)
(require 'dash)
(require 'json)
(require 'ido)

(setq chromedriver-connection nil)
(setq chromedriver-tab-id nil)

(defun extract-title-and-ws-address-pair (el)
  (let ((title (cdr (assoc 'title el)))
        (ws-address (cdr (assoc 'webSocketDebuggerUrl el))))
    (list title ws-address)))

(defun get-available-tabs ()
  (let ((response (json-read-from-string
                   (shell-command-to-string "curl -s http://localhost:9222/json"))))
    (-map 'extract-title-and-ws-address-pair response)))

(defun get-address-for (name)
  (nth 1 (car (-filter (lambda (el) (string= name (car el)))
                       (get-available-tabs)))))

(defun set-chromedriver-tab-id (address)
  (setq chromedriver-tab-id
        (substring address (+ 1 (string-match "/[0-9]+" address)))))

(defun set-new-ws-connection (tab-name)
  (let ((address (get-address-for tab-name)))
    (set-chromedriver-tab-id address)
    (setq chromedriver-connection
          (websocket-open address
          :on-open (lambda (ws) (message "Websocket opened"))
          :on-message (lambda (ws frame)
                        (message "ws frame: %S" (assoc 'result (assoc 'result (json-read-from-string (websocket-frame-payload frame))))))
          :on-close (lambda (ws) (message "Websocket closed"))))))

(defun connect-to-chrome ()
  (interactive)
  (let ((name (ido-completing-read "What tab do you want to connect to? " (get-available-tabs))))
    (message "Connecting to.. %s" name)
    (set-new-ws-connection name)))

;; (websocket-send-text ws (json-encode '(:id 6 :method "Page.reload")))

(defun eval-in-browser (expr)
  "Evaluate expresion in the web browser's console"
  (websocket-send-text ws (json-encode `(:id 6 :method "Runtime.evaluate" :params (:expression ,expr)))))

;; (eval-in-browser "console.log('szesc!');")
;; (eval-in-browser "function add(a,b) { return a+b; }; add(2, 10);")

;; (eval-in-browser "MM.categoryListView.children[0].model.get('name')")

(provide 'chromedriver)
