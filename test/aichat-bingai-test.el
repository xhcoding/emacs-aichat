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
(defun aichat-bingai-test-message-type-1 ()
  (aichat-json-parse-file (expand-file-name "test/aichat-bingai-message-type-1.json" (file-name-directory (locate-library "aichat-util")))))

(defun aichat-bingai-test-message-type-1-search-result ()
  (aichat-json-parse-file (expand-file-name "test/aichat-bingai-message-type-1-search-result.json" (file-name-directory (locate-library "aichat-util")))))

(defun aichat-bingai-test-message-type-2 ()
  (aichat-json-parse-file (expand-file-name "test/aichat-bingai-message-type-2.json" (file-name-directory (locate-library "aichat-util")))))

(defun aichat-bingai-test-message-type-2-image-prompt ()
  (aichat-json-parse-file (expand-file-name "test/aichat-bingai-message-type-2-image-prompt.json" (file-name-directory (locate-library "aichat-util")))))


(ert-deftest aichat-bingai-message-type-1-text ()
  (let ((message-type-1 (aichat-bingai-test-message-type-1)))
    (should (string= (aichat-bingai-message-type-1-text message-type-1) "根据Bing翻译[^1^]，这句话的英文是：I want to travel.😊\n"))))

(ert-deftest aichat-bingai-message-type-1-suggestion ()
  (let ((message (aichat-bingai-test-message-type-1)))
    (should (= (length (aichat-bingai-message-type-1-suggestion message)) 3))))

(ert-deftest aichat-bingai-message-type-2-text ()
  (let ((message-type-2 (aichat-bingai-test-message-type-2)))
    (should (string= (aichat-bingai-message-type-2-text message-type-2) "判断链表的长度是一个常见的数据结构和算法问题。有多种方法可以解决这个问题，我为你找到了一些相关的资料，你可以参考一下：\n\n- 一种方法是使用**循环**，即定义一个计数器和一个指针，从头节点开始遍历链表，每遍历一个节点，计数器加一，直到指针为空为止，此时计数器的值就是链表的长度[^3^] [^2^]。这种方法的时间复杂度是O (n)，空间复杂度是O (1)。\n- 另一种方法是使用**递归**，即定义一个递归函数，如果节点为空，返回0，否则返回1加上对下一个节点的递归调用，最终返回的值就是链表的长度[^3^]。这种方法的时间复杂度也是O (n)，但空间复杂度是O (n)，因为需要栈空间来存储递归调用。\n\n以上是我能找到的一些判断链表长度的方法，你可以根据自己的需要选择合适的方法。如果你还有其他问题，欢迎继续和我聊天。😊"))))

(ert-deftest aichat-bingai-message-type-2-suggestion ()
  (let ((message-type-2 (aichat-bingai-test-message-type-2)))
    (should (= (length (aichat-bingai-message-type-2-suggestion message-type-2)) 3))))

(ert-deftest aichat-bingai-message-type-2-image-prompt ()
  (let ((message-type-2 (aichat-bingai-test-message-type-2-image-prompt)))
    (should (string= (aichat-bingai-message-type-2-image-prompt message-type-2) "一只红色的猫和一只绿色的狗"))))

(ert-deftest aichat-bingai-message-type-2-source-attrs ()
  (let ((message-type-2 (aichat-bingai-test-message-type-2)))
    (should (= (length (aichat-bingai-message-type-2-source-attrs message-type-2)) 3))))

(provide 'aichat-bingai-test)
;;; aichat-bingai-test.el ends here
