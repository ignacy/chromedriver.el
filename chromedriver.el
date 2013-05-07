;;; chromedriver.el --- Control Google Chrome browser from Emacs

;; Copyright (C) 2013 Ignacy Moryc <imoryc@gmail.com>
;;
;; Author: Ignacy Moryc <imoryc@gmail.com>
;; Created: 10 April 2013
;; Version: 0.1

;; Commentary:

;; This is an early version

;; Usage:

;; Add you to your .emacs.d:

;; (add-to-list 'load-path "/path/to/chromedriver.el") ;; optional
;; (require 'chromedriver)

;; To allow websocket connections you need to run Chrome with
;; --remote-debugging-port option. Chromedriver assumes this port is set to 9222
;; For example on OS X you should close Google Chrome and then execute:
;; /Applications/Google Chrome.app/Contents/MacOS/Google Chrome --remote-debugging-port=9222

;; Chromedriver gives you two commands:
;; (connect-to-chrome) - you should do this before anything else
;; (reload-chrome-tab) - reloads currently connected tab

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'websocket)
(require 'dash)
(require 'json)
(require 'ido)

(defvar chromedriver-connection nil
  "Stores current connection")

(defvar chromedriver-tab-id nil
  "Stores the id number of the open tab")

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
        (string-to-number (substring address (+ 1 (string-match "/[0-9]+" address))))))

(defun log-frame (frame)
  (message "ws frame: %s" (json-read-from-string (websocket-frame-payload frame))))

(defun set-new-ws-connection (tab-name)
  (let ((address (get-address-for tab-name)))
    (when chromedriver-connection
      (websocket-close chromedriver-connection)
      (setq chromedriver-connection nil))
    (set-chromedriver-tab-id address)
    (setq chromedriver-connection
          (websocket-open
           address
          :on-open (lambda (ws) (message "Websocket opened"))
          :on-message (lambda (ws frame)
                        (log-frame frame))
          :on-close (lambda (ws) (message "Websocket closed"))))))

(defun connect-to-chrome ()
  "Connects to one of Chrome tabs"
  (interactive)
  (let ((name (ido-completing-read "What tab do you want to connect to? " (get-available-tabs))))
    (message "Connecting to.. %s" name)
    (set-new-ws-connection name)))

(defun reload-chrome-tab ()
  "Refreshes currently connected page"
  (interactive)
  (if chromedriver-connection
      (websocket-send-text chromedriver-connection
                           (json-encode `(:id ,chromedriver-tab-id :method "Page.reload")))
    (message "First connect to Chrome with `connect-to-chrome`")))

;; (defun eval-in-browser (expr)
;;   "Evaluate expresion in the web browser's console"
;;   (websocket-send-text chromedriver-connection
;;                        (json-encode `(:id ,chromedriver-tab-id :method "Runtime.evaluate" :params (:expression ,expr)))))
;;(eval-in-browser "console.log('szesc!');")
;;(eval-in-browser "function add(a,b) { return a+b; }; add(2, 10);")
;; (eval-in-browser "MM.categoryListView.children[0].model.get('name')")

(provide 'chromedriver)
