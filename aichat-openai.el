;;; aichat-openai.el --- aichat-openai   -*- lexical-binding: t; -*-

;; Filename: aichat-openai.el
;; Description: aichat-openai
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-03-10 12:34:27
;; Version: 0.1
;; Last-Updated: 2023-03-10 12:34:27
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
;; aichat-openai
;;

;;; Installation:
;;
;; Put aichat-openai.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'aichat-openai)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET aichat-openai RET
;;

;;; Change log:
;;
;; 2023/03/10
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

(require 'aichat-util)

;;; Code:

(defgroup aichat-openai nil
  "OpenAI in Emacs."
  :group 'aichat
  :prefix "aichat-openai-")

(defcustom aichat-openai-proxy nil
  "Http proxy of request openai api.

(setq aichat-openai-proxy \"localhost:51837\")"
  :group 'aichat-openai
  :type 'string)

(defun aichat-openai-api-key ()
  "Get openai api key from `auth-sources'."
  (auth-source-pick-first-password :host "platform.openai.com" :user "aichat-openai"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst aichat-openai--chat-completions-url "https://api.openai.com/v1/chat/completions" "The url of chat completions api.")

(defconst aichat-openai--chat-completions-model "gpt-3.5-turbo" "The model of chat completions api.")

(defconst aichat-openai--chat-completions-request-type "POST" "The request type of chat completions api.")

(defvar aichat-openai--http-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,(format "Bearer %s" (aichat-openai-api-key))))
  "The default headers of openai request.")

(cl-defun aichat-openai-make-chat-messages (&rest settings
                                                  &key
                                                  user
                                                  system
                                                  assistant)
  "Make chat completions messages.

(aichat-openai--make-chat-messages
 :user \"hello\"
 :assistant \"Hello there, how may I assist you today?\")"
  (vconcat
   (cl-loop for (key value) on settings by #'cddr
            collect (list :role (string-trim-left (symbol-name key) ":") :content value))))

(cl-defun aichat-openai-chat-completions (messages &rest settings
                                                   &key
                                                   on-success
                                                   on-error
                                                   &allow-other-keys
                                                   )
  "Request openai chat completions api.

Look https://platform.openai.com/docs/api-reference/chat."

  (let* ((args (cl-loop for (key value) on settings by #'cddr
                        append (unless (member key '(:on-success :on-error))
                                 (list key value))))
         (data (aichat-json-serialize (append
                                       (list
                                        :model aichat-openai--chat-completions-model
                                        :messages messages)
                                       args))))

    (aichat-debug "Request %s with data:\n%s\n" aichat-openai--chat-completions-url data)

    (promise-then (aichat-http aichat-openai--chat-completions-url
                               :proxy aichat-openai-proxy
                               :type aichat-openai--chat-completions-request-type
                               :headers aichat-openai--http-headers
                               :data data)
                  (lambda (value)
                    (aichat-debug "Received: %s" value)
                    (seq-let (status headers body) value
                      (if (string= "200" (car status))
                          (when on-success
                            (funcall on-success (aichat-json-parse body)))
                        (when on-error
                          (funcall on-error body)))))
                  (lambda (err)
                    (when on-error
                      (funcall on-error err))))))

(cl-defun aichat-openai-chat-completions-stream (messages callback &rest settings
                                                          &key
                                                          on-success
                                                          on-error
                                                          &allow-other-keys
                                                          )
  "Request openai chat completions api with stream mode.

Look https://platform.openai.com/docs/api-reference/chat for more request params."
  (let* ((args (cl-loop for (key value) on settings by #'cddr
                        append (unless (member key '(:on-success :on-error))
                                 (list key value))))
         (data (aichat-json-serialize (append
                                       (list
                                        :model aichat-openai--chat-completions-model
                                        :messages messages
                                        :stream t)
                                       args))))

    (aichat-debug "Request %s with data:\n%s\n" aichat-openai--chat-completions-url data)

    (promise-then (aichat-http-event-source aichat-openai--chat-completions-url
                                            (lambda (event-id event-data)
                                              (aichat-debug "Received event: %s, data: \n%s\n" event-id event-data)
                                              (unless (string= event-data "[DONE]")
                                                (when callback
                                                  (funcall callback (aichat-json-parse event-data)))))
                                            :proxy aichat-openai-proxy
                                            :type aichat-openai--chat-completions-request-type
                                            :headers aichat-openai--http-headers
                                            :data data)
                  (lambda (value)
                    (seq-let (status headers body) value
                      (if (string= "200" (car status))
                          (when on-success
                            (funcall on-success (aichat-json-parse body)))
                        (when on-error
                          (funcall on-error body)))))
                  (lambda (err)
                    (when on-error
                      (funcall on-error err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Message API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aichat-openai-chat-completions-content (msg)
  "[choices][0][message][content]"
  (aichat-json-access msg "{choices}[0]{message}{content}"))

(defun aichat-openai-chat-completions-delta-content (msg)
  "[choices][0][delta][content]"
  (aichat-json-access msg "{choices}[0]{delta}{content}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Chat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aichat-openai-chat-demo (say)
  "This is just a demo."
  (interactive "sYou say: ")
  (let ((buf (get-buffer-create "*chat-demo*")))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert say)
      (insert "\n"))
    (aichat-openai-chat-completions-stream
     (aichat-openai-make-chat-messages :user say)
     (lambda (msg)
       (let ((delta-content (aichat-openai-chat-completions-delta-content msg)))
         (when delta-content
           (with-current-buffer buf
             (goto-char (point-max))
             (insert delta-content)))))
     :on-error (lambda (err)
                 (message "error: %s"err)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Assistant ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom aichat-openai-assistant-buffer "*Aichat-OpenAI-Assistant*"
  "The buffer of show assistant message."
  :group 'aichat
  :type 'string)

(defcustom aichat-openai-assistant-display-function 'display-buffer
  "The function of display `aichat-openai-assistant-buffer'."
  :group 'aichat
  :type 'symbol)

(defun aichat-openai-assistant-get-buffer ()
  (get-buffer-create aichat-openai-assistant-buffer))

(defun aichat-openai-assistant (text)
  "Send the region or input to Bing and display the returned result to `aichat-openai-assistant-buffer'."
  (interactive (list (aichat-read-region-or-input "Input text: ")))
  (aichat-openai-chat-completions (aichat-openai-make-chat-messages
                                   :user text)
                                  :on-success (lambda (msg)
                                                (let ((content (aichat-openai-chat-completions-content msg))
                                                      (buffer (aichat-openai-assistant-get-buffer)))
                                                  (with-current-buffer buffer
                                                    (goto-char (point-max))
                                                    (insert content)
                                                    (insert "\n\n"))
                                                  (funcall aichat-openai-assistant-display-function buffer)))
                                  :on-error (lambda (err)
                                              (message "error: %s"err))))

(provide 'aichat-openai)

;;; aichat-openai.el ends here
