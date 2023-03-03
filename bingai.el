;;; bingai.el --- bingai.el   -*- lexical-binding: t; -*-

;; Filename: bingai.el
;; Description: bingai.el
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-02-28 17:49:20
;; Version: 0.1
;; Last-Updated: 2023-02-28 17:49:20
;;           By: xhcoding
;; URL: https://github.com/xhcoding/bingai
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
;; bingai.el
;;

;;; Installation:
;;
;; Put bingai.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'bingai)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET bingai RET
;;

;;; Change log:
;;
;; 2023/02/28
;;      * First released.
;;

;;; Acknowlbingments:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(eval-when-compile (require 'cl-lib))

(require 'json)
(require 'seq)
(require 'url)
(require 'websocket)
(require 'async-await)

;;; Hack

(defun websocket-create-headers (url key protocol extensions custom-headers-alist)
  "Create connections headers for the given URL, KEY, PROTOCOL, and EXTENSIONS.
Additionally, the CUSTOM-HEADERS-ALIST is passed from the client.
All these parameters are defined as in `websocket-open'."
  (let* ((parsed-url (url-generic-parse-url url))
         (host-port (if (url-port-if-non-default parsed-url)
                        (format "%s:%s" (url-host parsed-url) (url-port parsed-url))
                      (url-host parsed-url)))
         (cookie-header (url-cookie-generate-header-lines
                         host-port (car (url-path-and-query parsed-url))
                         (equal (url-type parsed-url) "wss"))))
    (concat
     (format (concat "Host: %s\r\n"
                     "Upgrade: websocket\r\n"
                     "Connection: Upgrade\r\n"
                     "Sec-WebSocket-Key: %s\r\n"
                     "Sec-WebSocket-Version: 13\r\n"
                     (when protocol
                       (concat
                        (mapconcat
                         (lambda (protocol)
                           (format "Sec-WebSocket-Protocol: %s" protocol))
                         protocol "\r\n")
                        "\r\n"))
                     (when extensions
                       (format "Sec-WebSocket-Extensions: %s\r\n"
                               (mapconcat
                                (lambda (ext)
                                  (concat
                                   (car ext)
                                   (when (cdr ext) "; ")
                                   (when (cdr ext)
                                     (mapconcat 'identity (cdr ext) "; "))))
                                extensions ", "))))
             host-port
             key
             protocol)
     (when cookie-header cookie-header)
     (concat (mapconcat (lambda (cons) (format "%s: %s" (car cons) (cdr cons)))
                        custom-headers-alist "\r\n")
             (when custom-headers-alist "\r\n"))
     "\r\n")))

;; setup url library, avoid set cookie failed
(url-do-setup)

;;; Code:

(defgroup bingai nil
  "Bing AI in Emacs."
  :group 'tools
  :prefix "bingai-")

(defcustom bingai-cookies-file nil
  "The path of www.bing.com cookies file.

When you set this value, bingai will login to www.bing.com through the cookies in the file."
  :group 'bingai
  :type 'string)

(defcustom bingai-debug nil
  "When set to `t', it will output more debug message in the *BINGAI-DEBUG* buffer."
  :group 'bingai
  :type 'boolean)

(defun bingai--debug (str &rest args)
  "Print debug message to *BINGAI-DEBUG* buffer when `bingai-debug' is set `t'"
  (when bingai-debug
    (with-current-buffer (get-buffer-create "*BINGAI-DEBUG*")
      (goto-char (point-max))
      (insert (apply #'format str args) "\n"))))

(defconst bingai--user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36 Edg/110.0.1587.57"
  "`bingai--user-agent' is used to set the value of User-Agent.")

(defconst bingai--url-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
       ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ?- ?_ ?. ?~)
  "`bingai--url-unreserved-chars' copied from Emacs 24.3 release candidate.
This is used for making `bingai--urlencode-alist' RFC 3986 compliant
for older Emacs versions.")

(defun bingai--urlencode-alist (alist)
  "Hexify ALIST fields according to RFC3986."
  (let ((url-unreserved-chars bingai--url-unreserved-chars))
    (cl-loop for sep = "" then "&"
             for (k . v) in alist
             concat sep
             concat (url-hexify-string (format "%s" k))
             concat "="
             concat (url-hexify-string (format "%s" v)))))

(cl-defun bingai--http (url &rest settings
                            &key
                            (type nil)
                            (params nil)
                            (headers nil)
                            (data nil))
  "Request URL with property list SETTINGS as follow. Return value is promise,
the format of resolve value is (resp-status resp-headers resp-body).

===================== ======================================================
Keyword argument      Explanation
===================== ======================================================
TYPE          (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS         (alist)   set \"?key=val\" part in URL
HEADERS        (alist)   additional headers to send with the request
DATA          (string)   data to be sent to the server
"
  (when params
    (cl-assert (listp params) nil "PARAMS must be an alist.  Given: %S" params)
    (setq url (concat url (if (string-match-p "\\?" url) "&" "?")
                      (bingai--urlencode-alist params))))
  (unless type
    (setq type "GET"))
  (promise-new (lambda (resolve reject)
                 (condition-case error
                     (let ((url-user-agent bingai--user-agent)
                           (url-request-extra-headers headers)
                           (url-request-method type)
                           (url-request-data data)
                           buf)
                       (url-retrieve url
                                     (lambda (_)
                                       (let (resp-status
                                             resp-headers
                                             resp-body)
                                         (goto-char (point-min))
                                         (when (re-search-forward "^HTTP/[1-9]\\.[0-9] \\([0-9]\\{3\\}\\) \\([a-zA-Z ]+\\)" url-http-end-of-headers t)
                                           (setq resp-status (cons (match-string 1) (match-string 2))))
                                         (while (re-search-forward "^\\([^:]*\\): \\(.+\\)" url-http-end-of-headers t)
                                           (push (cons (match-string 1) (match-string 2)) resp-headers))
                                         (goto-char (1+ url-http-end-of-headers))
                                         (setq resp-body (buffer-substring (point) (point-max)))
                                         (funcall resolve (list resp-status resp-headers resp-body))))
                                     nil t))
                   (error (funcall reject error))))))

(async-defun bingai--start-process (program &rest args)
  "Async start process with PROGRAM and ARGS.

Returns stdout on success, otherwise returns nil."
  (condition-case reason
      (car (await (promise:make-process-with-handler (cons program args) nil t)))
    (error nil)))

(async-defun bingai--shell-command (command &optional dir)
  "Async run COMMAND in DIR or `default-directory'.

Returns stdout on success, otherwise returns nil."
  (condition-case reason
      (let ((default-directory (or dir default-directory)))
        (await (promise:make-process-with-handler (list shell-file-name shell-command-switch command))))
    (error nil)))

(async-defun bingai--check-deps ()
  "Check if browser_cookie3 is installed."
  (when-let ((installed (await (bingai--shell-command "python -c \"import browser_cookie3\""))))
    t))

(defun bingai--get-cookies-from-file (filename)
  "Get `bingai--domain' cookies from FILENAME."
  (when (file-exists-p filename)
    (let ((cookies (json-read-file filename)))
      (mapcar (lambda (cookie)
                (let ((name (alist-get 'name cookie))
                      (value (alist-get 'value cookie))
                      (expires (if (assq 'expirationDate cookie)
                                   (format-time-string "%FT%T%z"
                                                       (seconds-to-time
                                                        (alist-get 'expirationDate cookie)))
                                 nil))
                      (domain (alist-get 'domain cookie))
                      (localpart (alist-get 'path cookie))
                      (secure (if (eq (alist-get 'secure cookie) :json-false)
                                  nil
                                t)))
                  (list name value expires domain localpart secure)))
              cookies))))

(defconst bingai--get-cookies-script
  "python -c \"import browser_cookie3;list(map(lambda c: print('{} {} {} {} {} {}'.format(c.name, c.value, c.expires, c.domain, c.path, c.secure)), filter(lambda c: c.domain in ('.bing.com', 'www.bing.com'), browser_cookie3.edge(domain_name='bing.com'))))\""
  "Shell script for get www.bing.com cookies.")

(async-defun bingai--get-cookies ()
  "Get `bingai--domain' cookies."
  (await nil)
  (if bingai-cookies-file
      (bingai--get-cookies-from-file bingai-cookies-file)
    (if (not (await (bingai--check-deps)))
        (message "Please install browser_cookie3 by `pip3 install browser_cookie3`")
      (when-let ((stdout (await
                          (promise:make-shell-command bingai--get-cookies-script))))
        (mapcar (lambda (line)
                  (let* ((fields (split-string line " " t))
                         (name (nth 0 fields))
                         (value (nth 1 fields))
                         (expires (if (string= (nth 2 fields) "None")
                                      nil
                                    (format-time-string "%FT%T%z" (seconds-to-time (string-to-number (nth 2 fields))))))
                         (domain (nth 3 fields))
                         (localpart (nth 4 fields))
                         (secure (if (string= (nth 5 fields) "1")
                                     t
                                   nil)))
                    (list name value expires domain localpart secure)))
                (split-string stdout "\n" t))))))


(defconst bingai--domain "bing.com"
  "Bing domain for retrieve cookies.")

(async-defun bingai--refresh-cookies ()
  "Refresh `bing--domain' cookies.

Delete all cookies from the cookie store where the domain matches `bing--domain'.
Re-fetching cookies from `bing--domain'"
  (when-let ((bing-cookies (await (bingai--get-cookies))))
    (bingai--debug "bing-cookies:\n%s\n" bing-cookies)
    (ignore-errors (url-cookie-delete-cookies bingai--domain))
    (dolist (bing-cookie bing-cookies)
      (apply #'url-cookie-store bing-cookie))))

(defun bingai--login-p ()
  "Check if you're already login."
  (when-let* ((host-cookies
               (seq-find (lambda (host)
                           (string= (car host) ".bing.com"))
                         (append url-cookie-secure-storage)))
              (user (seq-find
                     (lambda (cookie)
                       (string= (aref cookie 1) "_U"))
                     (cdr host-cookies))))
    (and user
         (not (url-cookie-expired-p user)))))

(async-defun bingai--login ()
  "Login www.bing.com."
  (await t)
  (unless (bingai--login-p)
    (await (bingai--refresh-cookies))))

(defun bingai--uuid ()
  "Return string with random (version 4) UUID."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			              (random)
			              (time-convert nil 'list)
			              (user-uid)
			              (emacs-pid)
			              (user-full-name)
			              user-mail-address
			              (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
	        (substring rnd 0 8)
	        (substring rnd 8 12)
	        (substring rnd 13 16)
	        (format "%x"
		            (logior
		             #b10000000
		             (logand
		              #b10111111
		              (string-to-number
		               (substring rnd 16 18) 16))))
	        (substring rnd 18 20)
	        (substring rnd 20 32))))

(defconst bingai--create-conversation-url "https://edgeservices.bing.com/edgesvc/turing/conversation/create"
  "The url of create conversation.")

(defconst bingai--headers
  `(("accept" . "application/json")
    ("accept-language" . "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6")
    ("sec-ch-ua" . "\"Chromium\";v=\"110\", \"Not A(Brand\";v=\"24\", \"Microsoft Edge\";v=\"110\"")
    ("sec-ch-ua-arch" . "\"x86\"")
    ("sec-ch-ua-bitness" . "\"64\"")
    ("sec-ch-ua-full-version" . "\"110.0.1587.57\"")
    ("sec-ch-ua-full-version-list" . "\"Chromium\";v=\"110.0.5481.178\", \"Not A(Brand\";v=\"24.0.0.0\", \"Microsoft Edge\";v=\"110.0.1587.57\"")
    ("sec-ch-ua-mobile" . "?0")
    ("sec-ch-ua-model" . "")
    ("sec-ch-ua-platform" . "\"Windows\"")
    ("sec-ch-ua-platform-version" . "\"15.0.0\"")
    ("sec-fetch-dest" . "empty")
    ("sec-fetch-mode" . "cors")
    ("sec-fetch-site" . "same-origin")
    ("x-ms-client-request-id" . ,(bingai--uuid))
    ("x-ms-useragent" . "azsdk-js-api-client-factory/1.0.0-beta.1 core-rest-pipeline/1.10.0 OS/Win32")
    ("referer" . "https://www.bing.com/search?q=Bing+AI&showconv=1")
    ("referrer-policy" . "origin-when-cross-origin"))
  "The headers of sending request to www.bing.com.")


(cl-defstruct (bingai--conversation
               (:constructor bingai--conversation-new)
               (:copier nil))
  "A conversation structure.
`id', `signature' and `client-id' are obtained through the GET request `bingai--conversation-url' response."
  id
  signature
  client-id)


(cl-defstruct (bingai--session
               (:constructor bingai--session-new)
               (:copier nil))
  conversation
  chathub
  (invocation-id 0)
  (replying nil)
  (buffer "")
  resolve
  reject
  user-cb
  )

(async-defun bingai--make-conversation ()
  (await (bingai--login))
  (seq-let
      (status headers body)
      (await (bingai--http bingai--create-conversation-url
                           :headers bingai--headers))
    (bingai--debug "status:\n%s\nheaders:\n%s\nbody:\n%s\n" status headers body)
    (if (not (string= "200" (car status)))
        (error "Create conversation failed: %s" status)
      (let* ((data (json-read-from-string body))
             (result-value (alist-get 'value (alist-get 'result data))))
        (if (not (string= "Success" result-value))
            (error "Create conversation failed: %s" body)
          (bingai--conversation-new
           :id (alist-get 'conversationId data)
           :signature (alist-get 'conversationSignature data)
           :client-id (alist-get 'clientId data)))))))


(defconst bingai--chathub-url "wss://sydney.bing.com/sydney/ChatHub")

(defconst bingai--message-delimiter (char-to-string #x1e))

(defun bingai--chathub-handle-message (session text)
  (bingai--debug "Recv text:\n%s" text)
  (let ((buffer (concat (bingai--session-buffer session) text))
        (start-pos 0)
        (match-pos nil)
        (object))
    (bingai--debug "buffer:\n%s" buffer)
    (catch 'not-find
      (while t
        (setq match-pos (string-match-p bingai--message-delimiter buffer start-pos))
        (if (not match-pos)
            (throw 'not-find match-pos)
          (setq object (json-read-from-string (substring buffer start-pos match-pos)))
          (bingai--debug "object:\n%s" object)
          (let* ((type (alist-get 'type object))
                 (user-cb (bingai--session-user-cb session))
                 (is-final (= type 3)))
            (when (and user-cb (= type 1))
              (funcall user-cb is-final object))
            (when is-final
              (setf (bingai--session-replying session) nil)
              (funcall (bingai--session-resolve session) t)))
          (setq start-pos (1+ match-pos)))))
    (setf (bingai--session-buffer session) (substring buffer start-pos))))

(defun bingai--make-chathub (session)
  (promise-new
   (lambda (resolve reject)
     (websocket-open bingai--chathub-url
                     :custom-header-alist bingai--headers
                     :on-open (lambda (ws)
                                (bingai--debug "====== chathub opened ======")
                                ;; send handshake
                                (websocket-send-text ws (concat (json-encode
                                                                 (list :protocol "json" :version 1))
                                                                bingai--message-delimiter)))
                     :on-close (lambda (_ws)
                                 (bingai--debug "====== chathub closed ======")
                                 (setf (bingai--session-chathub session) nil))
                     :on-message (lambda (ws frame)
                                   (let ((text (websocket-frame-text frame)))
                                     (condition-case error
                                         (progn
                                           (bingai--debug "Receive handshake response: %s" text)
                                           (json-read-from-string (car (split-string text bingai--message-delimiter)))
                                           (setf (websocket-on-message ws)
                                                 (lambda (_ws frame)
                                                   (bingai--chathub-handle-message session (websocket-frame-text frame))))
                                           (setf (bingai--session-chathub session) ws)
                                           (funcall resolve t))
                                       (error (funcall reject error)))))))))

(defun bingai--make-request (session question reply-cb)
  (promise-new
   (lambda (resolve reject)
     (when-let* ((conversation (bingai--session-conversation session))
                 (invocation-id (bingai--session-invocation-id session))
                 (request (list :arguments
                                (vector
                                 (list :source "cib"
                                       :optionsSets (vector "deepleo" "enable_debug_commands" "disable_emoji_spoken_text" "enablemm")
                                       :isStartOfSession (if (= 0 invocation-id)
                                                             t
                                                           :json-false)
                                       :message (list :author "user"
                                                      :inputMethod "Keyboard"
                                                      :text question
                                                      :messageType "Chat")
                                       :conversationSignature (bingai--conversation-signature conversation)
                                       :participant (list :id (bingai--conversation-client-id conversation))
                                       :conversationId (bingai--conversation-id conversation)))
                                :invocationId (number-to-string (bingai--session-invocation-id session))
                                :target "chat"
                                :type 4))
                 (result ""))
       (setq result (concat (json-encode request)  bingai--message-delimiter))

       (bingai--debug "Send text:\n%s\n" result)

       (websocket-send-text (bingai--session-chathub session) result)

       (setf (bingai--session-invocation-id session) (1+ invocation-id)
             (bingai--session-replying session) t
             (bingai--session-buffer session) ""
             (bingai--session-resolve session) resolve
             (bingai--session-reject session) reject
             (bingai--session-user-cb session) reply-cb)))))

(defvar bingai--current-session nil) ;; only one session

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bingai API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bingai--started-p ()
  (if bingai--current-session
      t
    nil))

(async-defun bingai--start ()
  (await t)
  (when (bingai--started-p)
    (bingai--stop bingai--current-session))
  (when-let ((conversation (await (bingai--make-conversation)))
             (session (bingai--session-new
                       :conversation conversation)))
    (setq bingai--current-session session)
    t))

(defun bingai--stop ()
  (when (bingai--started-p)
    (setf (bingai--session-conversation bingai--current-session) nil)
    (when-let ((chathub (bingai--session-chathub bingai--current-session)))
      (when (websocket-openp chathub)
        (websocket-close chathub)))
    (setf (bingai--session-invocation-id bingai--current-session) 0)
    (setq bingai--current-session nil)))

(async-defun bingai--say (question reply-cb)
  (unless (bingai--started-p)
    (await (bingai--start)))

  (unless (bingai--session-chathub bingai--current-session)
    (await (bingai--make-chathub bingai--current-session)))

  (if (bingai--session-replying bingai--current-session)
      (error "Please wait the last reply is over.")
    (await (bingai--make-request bingai--current-session question reply-cb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bingai msg API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bingai--msg-type-1-text (msg)
  (when (= 1 (alist-get 'type msg))
    (alist-get 'text
               (aref
                (alist-get 'body
                           (aref
                            (alist-get 'adaptiveCards
                                       (aref
                                        (alist-get 'messages
                                                   (aref (alist-get 'arguments msg) 0))
                                        0))
                            0))
                0))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; User API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bingai-file (expand-file-name "aichat.md" user-emacs-directory))

;;;###autoload
(defun bingai-toggle-debug ()
  (interactive)
  (cond
   (bingai-debug
    (setq bingai-debug nil
          url-debug nil
          websocket-debug nil))
   (t
    (setq bingai-debug t
          url-debug t
          websocket-debug t))))

;;;###autoload
(defun bingai-chat (say)
  (interactive "sYou say: ")
  (when (and (car current-prefix-arg)
             (= (car current-prefix-arg) 4))
    (bingai--stop))
  (let ((chat-buf (get-file-buffer bingai-file))
        (reply-length 0))
    (when (not chat-buf)
      (setq chat-buf (find-file-noselect bingai-file)))
    (switch-to-buffer chat-buf)
    (with-current-buffer chat-buf
      (goto-char (point-max))
      (insert (format "\n# %s\n\n" say)))
    (promise-then (bingai--say say
                               (lambda (final msg)
                                 (let ((type (alist-get 'type msg))
                                       (text)
                                       (insert-text))
                                   (cond
                                    ((= 1 type)
                                     (setq text (bingai--msg-type-1-text msg))
                                     (setq insert-text (substring text reply-length))
                                     (setq reply-length (length text))
                                     (with-current-buffer chat-buf
                                       (mapc #'insert insert-text)))))))
                  (lambda (_)
                    (with-current-buffer chat-buf
                      (insert "\n\n")
                      )
                    (message "Finished"))
                  (lambda (error-msg)
                    (message "error: %s" error-msg)))))

(provide 'bingai)

;;; bingai.el ends here
