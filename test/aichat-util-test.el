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
      (while t (accept-process-output nil 30)))))

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


(ert-deftest aichat-http-get ()
  (seq-let (status headers body)
      (promise-wait-value
       (promise-wait 100
         (aichat-http "https://httpbin.org/get"
                      :params '(("hello". "world")))))
    (should (string= "200" (car status)))
    (let ((body-object (json-read-from-string body)))
      (should (equal (alist-get 'args body-object) '((hello ."world"))))
      (should (string= (alist-get 'User-Agent (alist-get 'headers body-object)) aichat-user-agent)))))

(ert-deftest aichat-http-get-with-proxy ()
  (seq-let (status headers body)
      (should-error
       (promise-wait-value
        (promise-wait 100
          (aichat-http "https://httpbin.org/get"
                       :params '(("hello". "world"))
                       :proxy "error-proxy"))))))

(ert-deftest aichat-http-post ()
  (seq-let (status headers body)
      (promise-wait-value
       (promise-wait 100
         (aichat-http "https://httpbin.org/post"
                      :type "POST"
                      :headers '(("Content-Type" . "application/json")
                                 ("Authorization" . "Bearer sk-qZbRi"))
                      :data (json-encode (list :model "gpt-3.5-turbo"
                                               :stream t
                                               :messages (vector
                                                          (list :role "user"
                                                                :content "Hello, 我是你的助理" )))))))
    (should (string= "200" (car status)))
    (let ((body-object (json-read-from-string body)))
      (should (string= (alist-get 'User-Agent (alist-get 'headers body-object)) aichat-user-agent)))))


(ert-deftest aichat-http-event-source ()
  (let* ((datas)
         (server-file (expand-file-name "test/server.js" (file-name-directory (locate-library "aichat-util"))))
         (proc (start-process "event-server" nil "node" server-file)))
    (when proc
      (sleep-for 2)
      (promise-wait 10 (aichat-http-event-source "http://127.0.0.1:12345/stream"
                                                 (lambda (id data)
                                                   (push (cons id data) datas))))
      (should (= 3 (length datas)))
      (should (equal (cons "test" "this is data\nthis is new-line") (nth 0 datas)))
      (should (equal (cons "test" "this is data\nthis is new-line") (nth 1 datas)))
      (should (equal (cons "test" "this is data\nthis is new-line") (nth 2 datas))))))

(provide 'aichat-util-test)

;;; aichat-util-test.el ends here
