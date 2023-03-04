;;; bingai-test.el --- bingai-test.el   -*- lexical-binding: t; -*-

;; Filename: bingai-test.el
;; Description: bingai-test.el
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-03-04 22:46:50
;; Version: 0.1
;; Last-Updated: 2023-03-04 22:46:50
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
;; bingai-test.el
;; 

;;; Installation:
;;
;; Put bingai-test.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'bingai-test)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET bingai-test RET
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

;;; Code:

(ert-deftest aichat-ci ()
  (should (= 1 1)))

(provide 'bingai-test)

;;; bingai-test.el ends here
