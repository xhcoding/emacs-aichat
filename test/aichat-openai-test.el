;;; aichat-openai-test.el --- aichat-openai-test   -*- lexical-binding: t; -*-

;; Filename: aichat-openai-test.el
;; Description: aichat-openai-test
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-04-10 10:50:49
;; Version: 0.1
;; Last-Updated: 2023-04-10 10:50:49
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
;; aichat-openai-test
;;

;;; Installation:
;;
;; Put aichat-openai-test.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'aichat-openai-test)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET aichat-openai-test RET
;;

;;; Change log:
;;
;; 2023/04/10
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

(require 'aichat-openai)

;;; Code:

(ert-deftest aichat-openai-chat--buffer-messages-test ()
  (let* ((buffer (find-file-noselect "aichat-openai-chat-buffer.aichat"))
         (result (aichat-openai-chat--buffer-messages buffer)))
    (should (equal result '(:system "You are a helpful assistant." :user "Who won the world series in 2020?" :assistant "The Los Angeles Dodgers won the World Series in 2020.")))))

(ert-deftest aichat-openai-chat--heading-messages-test ()
  (let* ((buffer (find-file-noselect "aichat-openai-chat-buffer.aichat"))
         (result (aichat-openai-chat--heading-messages buffer)))
    (should (equal result '(:assistant "The Los Angeles Dodgers won the World Series in 2020.")))))

(provide 'aichat-openai-test)

;;; aichat-openai-test.el ends here
