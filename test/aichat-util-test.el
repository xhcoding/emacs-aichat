;;; aichat-util-test.el --- aichat-util-test.el   -*- lexical-binding: t; -*-

;; Filename: aichat-util-test.el
;; Description: aichat-util-test.el
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-03-04 23:16:38
;; Version: 0.1
;; Last-Updated: 2023-03-04 23:16:38
;;           By: xhcoding
;; URL: https://github.com/xhcoding/emacs-aichat
;; Keywords:
;; Compatibility: GNU Emacs 30.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; aichat-util-test.el
;;

;;; Installation:
;;
;; Put aichat-util-test.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'aichat-util-test)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET aichat-util-test RET
;;

;;; Change log:
;;
;; 2023/03/04
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'ert)
(require 'async-await)

(add-to-list 'load-path (expand-file-name ".." default-directory))

(require 'aichat-util)

;;; Hack

(defun promise-wait (timeout promise)
  "Return promise to wait synchronously until PROMISE is resolved or rejected or TIMEOUT.

Arguments:
  - TIMEOUT can accept the various formats.  See `run-at-time'.
  - PROMISE is any promise object.

Resolve:
  - Return (:fullfilled value), value is PROMISE resolved value.

Reject:
  - Return (:rejected reason), reason is PROMISE rejected reason.

Timeout:
  - Return (:timeouted)."
  (declare (indent 1))
  (catch 'done
    (let* (handled
           (timer (run-at-time timeout nil
                               (lambda ()
                                 (unless handled
                                   (setq handled t)
                                   (throw 'done (promise-reject '(:timeouted))))))))
      (promise-then promise
                    (lambda (value)
                      (unless handled
                        (setq handled t)
                        (cancel-timer timer)
                        (throw 'done (promise-resolve `(:fullfilled ,value)))))
                    (lambda (reason)
                      (unless handled
                        (setq handled t)
                        (cancel-timer timer)
                        (throw 'done (promise-reject `(:rejected ,reason))))))
      (while t (sit-for 20)))))

;;; Code:

(ert-deftest aichat-debug ()
  (let ((buffer-name "*AICHAT-DEBUG*")
        (msg "debug message"))
    (let ((aichat-debug t))
      (aichat-debug msg)
      (should (string= (concat msg "\n") (with-current-buffer buffer-name
                                           (buffer-string))))
      (kill-buffer buffer-name))
    (let ((aichat-debug nil))
      (aichat-debug msg)
      (should (not (get-buffer buffer-name))))))

(defconst aichat-util-test-json-str "{\"user\":\"xhcoding\",\"age\":1000,\"others\":[\"hello\",1],\"keys\":{\"key\":\"value\"}}")

(ert-deftest aichat-json-serialize ()
  (let ((str "hel\"lo"))
    (should (string= "\"hel\\\"lo\"" (aichat-json-serialize str))))
  (should (string= aichat-util-test-json-str
                   (aichat-json-serialize (list :user "xhcoding" :age 1000 :others ["hello" 1] :keys (list :key "value"))))))

(ert-deftest aichat-json-parse ()
  (should (equal (aichat-json-parse aichat-util-test-json-str)
                 '((user . "xhcoding") (age . 1000) (others . ["hello" 1]) (keys  (key . "value"))))))

(ert-deftest aichat-json-access ()
  (let ((object (aichat-json-parse aichat-util-test-json-str)))
    (should (string= "xhcoding" (aichat-json-access object "{user}")))
    (should (= 1000 (aichat-json-access object "{age}")))
    (should (string= "hello" (aichat-json-access object "{others}[0]")))
    (should (= 1 (aichat-json-access object "{others}[1]")))
    (should (string= "value" (aichat-json-access object "{keys}{key}")))
    (should (equal ["hello" 1] (aichat-json-access object "{others}")))))

(ert-deftest aichat-get-cookies-from-file()
  (let ((cookies (aichat-get-cookies-from-file (expand-file-name "test/cookies.json" (file-name-directory (locate-library "aichat-util"))))))
    (cl-loop for cookie in cookies
             do (let ((name (car cookie))
                      (value (cadr cookie)))
                  (when (string= name "_U")
                    (should (string= value "invalid, do not use this")))))))


(defun aichat-http-get-with-backend (backend)
  (seq-let (status headers body)
      (promise-wait-value
       (promise-wait 100
         (aichat-http "https://httpbin.org/get"
                      :backend backend
                      :params '(("hello". "world")))))
    (should (string= "200" (car status)))
    (let ((body-object (aichat-json-parse body)))
      (should (equal (aichat-json-access body-object "{args}") '((hello ."world"))))
      (should (string= (aichat-json-access body-object "{headers}{User-Agent}") aichat-user-agent)))))

;; (ert-deftest aichat-http-get ()
;;   (aichat-http-get-with-backend 'curl)
;;   (aichat-http-get-with-backend 'url))

(defun aichat-http-get-with-proxy-with-backend (backend)
  (seq-let (status headers body)
      (should-error
       (promise-wait-value
        (promise-wait 100
          (aichat-http "https://httpbin.org/get"
                       :backend backend
                       :params '(("hello". "world"))
                       :proxy "error-proxy"))))))

(ert-deftest aichat-http-get-with-proxy ()
  (aichat-http-get-with-proxy-with-backend 'curl)
  (aichat-http-get-with-proxy-with-backend 'url))

(defun aichat-http-get-with-cookie-with-backend (backend)
  (url-cookie-store "test" "aichat-http-get-with-cookie" nil "httpbin.org" "/" t)
  (seq-let (status headers body)
      (promise-wait-value
       (promise-wait 100
         (aichat-http "https://httpbin.org/get"
                      :backend 'curl
                      :params '(("hello". "world")))))
    (should (string= "200" (car status)))
    (let ((body-object (aichat-json-parse body)))
      (should (equal (aichat-json-access body-object "{args}") '((hello ."world"))))
      (should (string= (aichat-json-access body-object "{headers}{User-Agent}") aichat-user-agent))
      (should-not (string-empty-p (aichat-json-access body-object "{headers}{Cookie}"))))))

;; (ert-deftest aichat-http-get-with-cookie ()
;;   (aichat-http-get-with-cookie-with-backend 'curl)
;;   (aichat-http-get-with-cookie-with-backend 'url))

(defun aichat-http-post-with-backend (backend)
  (let ((data (aichat-json-serialize (list :model "gpt-3.5-turbo"
                                           :stream t
                                           :messages (vector
                                                      (list :role "user"
                                                            :content "Hello, 我是你的助理" ))))))
    (seq-let (status headers body)
        (promise-wait-value
         (promise-wait 100
           (aichat-http "https://httpbin.org/post"
                        :backend backend
                        :type "POST"
                        :headers '(("Content-Type" . "application/json")
                                   ("Authorization" . "Bearer sk-qZbRi"))
                        :data data)))
      (should (string= "200" (car status)))
      (let ((body-object (aichat-json-parse body)))
        (should (string= data (aichat-json-access body-object "{data}")))))))

;; (ert-deftest aichat-http-post ()
;;   (aichat-http-post-with-backend 'curl)
;;   (aichat-http-post-with-backend 'url))

(defun aichat-http-event-source-with-backend (backend)
  (let* ((datas)
         (server-file (expand-file-name "test/server.js" (file-name-directory (locate-library "aichat-util"))))
         (proc (start-process "event-server" nil "node" server-file)))
    (when proc
      (sleep-for 2)
      (promise-wait 10 (aichat-http-event-source "http://127.0.0.1:12345/stream"
                                                 (lambda (id data)
                                                   (push (cons id data) datas))
                                                 :backend backend))
      (should (= 3 (length datas)))
      (should (equal (cons "test" "this is data\nthis is new-line") (nth 0 datas)))
      (should (equal (cons "test" "this is data\nthis is new-line") (nth 1 datas)))
      (should (equal (cons "test" "this is data\nthis is new-line") (nth 2 datas))))))

(ert-deftest aichat-http-event-source ()
  (aichat-http-event-source-with-backend 'curl)
  (aichat-http-event-source-with-backend 'url))

(provide 'aichat-util-test)

;;; aichat-util-test.el ends here
