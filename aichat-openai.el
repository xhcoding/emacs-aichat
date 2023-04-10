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

(require 'markdown-mode)

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

(defun aichat-openai--default-api-key-function ()
  "Fetch the API key with auth-source."
  (auth-source-pick-first-password :host "platform.openai.com"))

(defcustom aichat-openai-api-key #'aichat-openai--default-api-key-function
  "OpenAI key as a string or a function that loads and returns it."
  :group 'aichat-openai
  :type '(choice (function :tag "Function")
                 (string :tag "String")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom aichat-openai-domain "https://api.openai.com"
  "OpenAI domain."
  :group 'aichat-openai
  :type 'string)

(defconst aichat-openai--chat-completions-url "/v1/chat/completions" "The url of chat completions api.")

(defconst aichat-openai--chat-completions-model "gpt-3.5-turbo" "The model of chat completions api.")

(defconst aichat-openai--chat-completions-request-type "POST" "The request type of chat completions api.")

(defun aichat-openai--make-http-headers ()
  "Create http headers to send in requests to the API."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format "Bearer %s" (if (functionp aichat-openai-api-key)
                                                (funcall aichat-openai-api-key)
                                              aichat-openai-api-key)))))

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

    (promise-then (aichat-http (concat aichat-openai-domain aichat-openai--chat-completions-url)
                               :proxy aichat-openai-proxy
                               :type aichat-openai--chat-completions-request-type
                               :headers (aichat-openai--make-http-headers)
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

    (promise-then (aichat-http-event-source (concat aichat-openai-domain aichat-openai--chat-completions-url)
                                            (lambda (event-id event-data)
                                              (aichat-debug "Received event: %s, data: \n%s\n" event-id event-data)
                                              (unless (string= event-data "[DONE]")
                                                (when callback
                                                  (funcall callback (aichat-json-parse event-data)))))
                                            :proxy aichat-openai-proxy
                                            :type aichat-openai--chat-completions-request-type
                                            :headers (aichat-openai--make-http-headers)
                                            :data data)
                  (lambda (value)
                    (seq-let (status headers body) value
                      (if (string= "200" (car status))
                          (when on-success
                            (funcall on-success body))
                        (when on-error
                          (funcall on-error body)))))
                  (lambda (err)
                    (when on-error
                      (funcall on-error err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Message API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aichat-openai-chat-completions-content (msg)
  "[choices][0][message][content]"
  (aichat-json-access msg "{choices}[0]{message}{content}"))

(defun aichat-openai-chat-completions-delta-content (msg)
  "[choices][0][delta][content]"
  (aichat-json-access msg "{choices}[0]{delta}{content}"))

(defun aichat-openai-chat-completions-delta-role (msg)
  "[choices][0][delta][role]"
  (aichat-json-access msg "{choices}[0]{delta}{role}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Chat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom aichat-openai-chat-directory (expand-file-name "aichat/openai" user-emacs-directory)
  "The directory of save chat file."
  :group 'aichat-openai
  :type 'string)

(defcustom aichat-openai-chat-display-function 'switch-to-buffer
  "The function of display chat buffer."
  :group 'aichat-openai
  :type 'symbol)

(defun aichat-openai-chat--buffer-messages (buffer)
  "Make messages with BUFFER's content."
  (with-current-buffer buffer
    (save-mark-and-excursion
      (goto-char (point-min))
      (let ((messages (list))
            (role)
            (message-beg)
            (message-end)
            (message))
        (catch 'finished
          (while t
            (if (re-search-forward "^# \\([a-zA-Z]+\\)\n" nil t)
                (setq role (downcase (string-trim (match-string 1))))
              (throw 'finished nil))
            (setq message-beg (point))
            (if (re-search-forward "^#" nil t)
                (progn
                  (backward-char)
                  (setq message-end (point)))
              (setq message-end (point-max)))
            (setq message (string-trim (buffer-substring-no-properties message-beg message-end)))
            (push (intern (concat ":" role)) messages)
            (push message messages)))
        (reverse messages)))))

(defun aichat-openai-chat--heading-messages (buffer)
  "Make messages with BUFFER's last heading content."
  (with-current-buffer buffer
    (save-mark-and-excursion
      (goto-char (point-max))
      (when (re-search-backward "^# " nil t)
        (save-restriction
          (narrow-to-region (point) (point-max))
          (aichat-openai-chat--buffer-messages (current-buffer)))))))

(defun aichat-openai-chat--send-messages (messages buffer)
  "Send MESSAGES to OpenAI, insert response to BUFFER."
  (aichat-openai-chat-completions-stream
   (apply #'aichat-openai-make-chat-messages messages)
   (lambda (msg)
     (let ((delta-role (aichat-openai-chat-completions-delta-role msg))
           (delta-content (aichat-openai-chat-completions-delta-content msg)))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (save-mark-and-excursion
             (goto-char (point-max))
             (when delta-role
               (insert (format "\n# %s%s\n\n" (upcase (substring delta-role 0 1)) (substring delta-role 1))))
             (when delta-content
               (insert delta-content)))))))
   :on-success (lambda (_)
                 (when (buffer-live-p buffer)
                   (goto-char (point-max))
                   (insert "\n\n# User\n\n")))
   :on-error (lambda (err)
               (message "error: %s"err))))

(defun aichat-openai-chat-send-buffer ()
  "Send current buffer content to OpenAI."
  (interactive)
  (when-let* ((buffer (current-buffer))
              (messages (aichat-openai-chat--buffer-messages buffer)))
    (aichat-openai-chat--send-messages messages buffer)))

(defun aichat-openai-chat-send-last-heading ()
  "Send last heading content to OpenAI."
  (interactive)
  (when-let* ((buffer (current-buffer))
              (messages (aichat-openai-chat--heading-messages buffer)))
    (aichat-openai-chat--send-messages messages buffer)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.aichat\\'" . aichat-openai-chat-mode))

;;;###autoload
(define-derived-mode aichat-openai-chat-mode markdown-mode "aichat-openai-chat"
  "Major mode for openai chat."
  (setq-local markdown-hide-markup t)
  (when (string-empty-p (buffer-string))
    (insert "# User\n"))
  (define-key aichat-openai-chat-mode-map (kbd "C-c C-c") #'aichat-openai-chat-send-buffer)
  (define-key aichat-openai-chat-mode-map (kbd "C-c C-l") #'aichat-openai-chat-send-last-heading))

;;;###autoload
(defun aichat-openai-chat (buffer)
  "If the chat buffer is not found, create a new chat buffer, otherwise switch to the opened chat buffer."
  (interactive (list (let ((opened-buffers (cl-loop for buffer in (buffer-list)
                                                    append (with-current-buffer buffer
                                                             (when (derived-mode-p 'aichat-openai-chat-mode)
                                                               (list (buffer-name)))))))
                       (if (and opened-buffers (not (and (car current-prefix-arg)
                                                         (= (car current-prefix-arg) 4))))
                           (if (= 1 (length opened-buffers))
                               (car opened-buffers)
                             (completing-read "Select buffer: " opened-buffers))
                         (when-let* ((name
                                      (completing-read "Chat name: "
                                                       (directory-files aichat-openai-chat-directory nil
                                                                        "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))
                                     (filename (expand-file-name (if (string-suffix-p ".aichat" name) name
                                                                   (concat name ".aichat"))
                                                                 aichat-openai-chat-directory)))
                           (make-directory aichat-openai-chat-directory t)
                           (find-file-noselect filename))))))
  (funcall aichat-openai-chat-display-function buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Assistant ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun aichat-openai-replace-or-insert (text)
  "Send the region or input to OpenAI and replace the selected region or insert at the current position with the returned result."
  (interactive (list (aichat-read-region-or-input "Input text: ")))
  (when (and text (not (string-empty-p text)))
    (let* ((cur-buf (current-buffer))
           (cur-pos (with-current-buffer cur-buf (point)))
           (reg-beg (when (use-region-p) (region-beginning)))
           (reg-end (when (use-region-p) (region-end))))
      (aichat-openai-chat-completions (aichat-openai-make-chat-messages
                                       :user text)
                                      :on-success (lambda (msg)
                                                    (when-let ((content (aichat-openai-chat-completions-content msg)))
                                                      (with-current-buffer cur-buf
                                                        (if (and reg-beg reg-end)
                                                            (replace-region-contents reg-beg reg-end (lambda () content))
                                                          (goto-char cur-pos)
                                                          (insert content)))))
                                      :on-error (lambda (err)
                                                  (message "Error: %s" err))))))

(cl-defmacro aichat-openai-prompt-create (name &rest args
                                               &key
                                               (input-prompt "Input text: ")
                                               (text-format "%s")
                                               (assistant nil)
                                               (replace-or-insert nil))
  "This macro will generate two functions: aichat-openai-assistant-name or aichat-openai-replace-or-insert-name.

INPUT-PROMPT: The prompt before the user input in minibuffer.
TEXT-FORMAT: Formating string, %s is replaced by what the user input."
  (let ((assistant-func (intern (format "aichat-openai-assistant-%s" name)))
        (replace-func (intern (format "aichat-openai-replace-or-insert-%s" name))))
    `(progn
       (when ,assistant
         (defun ,assistant-func(text)
           (interactive (list (aichat-read-region-or-input ,input-prompt)))
           (when text
             (let ((query (format ,text-format text)))
               (aichat-openai-assistant query)))))
       (when ,replace-or-insert
         (defun ,replace-func(text)
           (interactive (list (aichat-read-region-or-input ,input-prompt)))
           (when text
             (let ((query (format ,text-format text)))
               (aichat-openai-replace-or-insert query))))))))

(provide 'aichat-openai)

;;; aichat-openai.el ends here
