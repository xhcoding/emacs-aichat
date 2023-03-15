;;; aichat-bingai-test.el --- aichat-bingai-test   -*- lexical-binding: t; -*-

;; Filename: aichat-bingai-test.el
;; Description: aichat-bingai-test
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-03-12 10:23:01
;; Version: 0.1
;; Last-Updated: 2023-03-12 10:23:01
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
;; aichat-bingai-test
;;

;;; Installation:
;;
;; Put aichat-bingai-test.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'aichat-bingai-test)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET aichat-bingai-test RET
;;

;;; Change log:
;;
;; 2023/03/12
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

(add-to-list 'load-path (expand-file-name ".." default-directory))

(require 'aichat-bingai)

;;; Code:

(ert-deftest aichat-bingai--get-cookies-from-file()
  (let ((cookies (aichat-bingai--get-cookies-from-file (expand-file-name "test/cookies.json" (file-name-directory (locate-library "aichat-util"))))))
    (cl-loop for cookie in cookies
             do (let ((name (car cookie))
                      (value (cadr cookie)))
                  (when (string= name "_U")
                    (should (string= value "invalid, do not use this")))))))

(defun aichat-bingai-test-message-type-1 ()
  (aichat-json-parse-file (expand-file-name "test/aichat-bingai-message-type-1.json" (file-name-directory (locate-library "aichat-util")))))

(defun aichat-bingai-test-message-type-1-search-result ()
  (aichat-json-parse-file (expand-file-name "test/aichat-bingai-message-type-1-search-result.json" (file-name-directory (locate-library "aichat-util")))))

(defun aichat-bingai-test-message-type-2 ()
  (aichat-json-parse-file (expand-file-name "test/aichat-bingai-message-type-2.json" (file-name-directory (locate-library "aichat-util")))))


(ert-deftest aichat-bingai-message-type-1-text ()
  (let ((message-type-1 (aichat-bingai-test-message-type-1)))
    (should (string= (aichat-bingai-message-type-1-text message-type-1) "根据Bing翻译[^1^]，这句话的英文是：I want to travel.😊\n"))))

(ert-deftest aichat-bingai-message-type-1-search-result ()
  (let ((message (aichat-bingai-test-message-type-1-search-result)))
    (should (length= (aichat-bingai-message-type-1-search-result message) 4))))

(ert-deftest aichat-bingai-message-type-1-suggestion ()
  (let ((message (aichat-bingai-test-message-type-1)))
    (should (length= (aichat-bingai-message-type-1-suggestion message) 3))))

(ert-deftest aichat-bingai-message-type-2-text ()
  (let ((message-type-2 (aichat-bingai-test-message-type-2)))
    (should (string= (aichat-bingai-message-type-2-text message-type-2) "根据Bing翻译[^1^]，这句话的英文是：I want to travel.😊"))))

(ert-deftest aichat-bingai-message-type-2-suggestion ()
  (let ((message-type-2 (aichat-bingai-test-message-type-2)))
    (should (length= (aichat-bingai-message-type-2-suggestion message-type-2) 3))))

(provide 'aichat-bingai-test)
;;; aichat-bingai-test.el ends here
