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

(defcustom bingai-conversation-style 'balanced
  "Conversation style."
  :group 'bingai
  :type '(radio
          (const :tag "More Creative" creative)
          (const :tag "More Balanced" balanced)
          (const :tag "More Precise" precise)))

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
                           (url-request-data data))
                       (with-current-buffer
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
                                         nil t)
                         (set (make-local-variable 'url-user-agent) bingai--user-agent)))
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

(async-defun bingai--create-conversation ()
  "Create a conversation through the GET request `bingai--conversation-url'."
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

(cl-defstruct (bingai--session
               (:constructor bingai--session-new)
               (:copier nil))
  "A session structure.
`conversation' represents the `bingai--conversation'.
`chathub' represents the chathub websocket connection.
`invocation-id' indicates the number of questions.
`replying' indicates whether the reply is in progress.
`buffer' saves the reply message for parsing.
`resolve' and `reject' are promise callback, call `resolve' when the reply ends
and call `reject' when error occurs.
Call `user-cb' when a message arrives."
  conversation
  chathub
  (invocation-id 0)
  (replying nil)
  (buffer "")
  resolve
  reject
  user-cb
  )


(defconst bingai--message-delimiter (char-to-string #x1e)
  "Websocket json message delimiter.")

(defun bingai--chathub-parse-message (session text)
  "Parse chathub websocket json message."
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
              (condition-case error
                  (funcall user-cb object)
                (error
                 (setf (bingai--session-replying session) nil)
                 (websocket-close (bingai--session-chathub session))
                 (funcall (bingai--session-reject session) (format "User callback error: %s\n" error)))))
            (when is-final
              (setf (bingai--session-replying session) nil)
              (funcall (bingai--session-resolve session) t)))
          (setq start-pos (1+ match-pos)))))
    (setf (bingai--session-buffer session) (substring buffer start-pos))))

(defconst bingai--chathub-url "wss://sydney.bing.com/sydney/ChatHub"
  "The url of create chathub.")

(defun bingai--create-chathub (session)
  "Create a websocket connection to `bingai--chathub-url'.

Call resolve when the handshake with chathub passed."
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
                                 (setf (bingai--session-chathub session) nil)
                                 (when (bingai--session-replying session)
                                   ;; close when replying
                                   (setf (bingai--session-replying session) nil)
                                   (funcall (bingai--session-reject session) "Chathub closed unexpectedly during reply.")))
                     :on-message (lambda (ws frame)
                                   (let ((text (websocket-frame-text frame)))
                                     (condition-case error
                                         (progn
                                           (bingai--debug "Receive handshake response: %s" text)
                                           (json-read-from-string (car (split-string text bingai--message-delimiter)))
                                           (setf (websocket-on-message ws)
                                                 (lambda (_ws frame)
                                                   (bingai--chathub-parse-message session (websocket-frame-text frame))))
                                           (setf (bingai--session-chathub session) ws)
                                           (funcall resolve t))
                                       (error (funcall reject error)))))))))

(defun bingai--destroy-chathub (session)
  (when-let ((chathub (bingai--session-chathub bingai--current-session)))
    (when (websocket-openp chathub)
      (websocket-close chathub))))


(defun bingai--reply-options ()
  (vector
   "nlu_direct_response_filter"
   "deepleo"
   "disable_emoji_spoken_text"
   "responsible_ai_policy_2235"
   "enablemm"
   "rai253"
   "cricinfo"
   "cricinfov2"
   "dv3sugg"
   (pcase bingai-conversation-style
     ('creative "h3imaginative")
     ('balanced "harmonyv3")
     ('precise "h3precise"))))

(defconst bingai--allowed-message-types
  [
   "Chat"
   "InternalSearchQuery"
   "InternalSearchResult"
   "Disengaged"
   "InternalLoaderMessage"
   "RenderCardRequest"
   "AdsQuery"
   "SemanticSerp"
   "GenerateContentQuery"
   ])

(defconst binai--slice-ids
  [
   "h3adss0"
   "301rai253"
   "225cricinfo"
   "224locals0"
   ])

(defun bingai--send-said (session said reply-cb)
  "Send what the user said."
  (promise-new
   (lambda (resolve reject)
     (when-let* ((conversation (bingai--session-conversation session))
                 (invocation-id (bingai--session-invocation-id session))
                 (request (list :arguments
                                (vector
                                 (list :source "cib"
                                       :optionsSets (bingai--reply-options)
                                       :allowedMessageTypes bingai--allowed-message-types
                                       :sliceIds binai--slice-ids
                                       :isStartOfSession (if (= 0 invocation-id)
                                                             t
                                                           :json-false)
                                       :message (list :author "user"
                                                      :inputMethod "Keyboard"
                                                      :text said
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

(defvar bingai--current-session nil  ;; only one session
  "Bingai session.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bingai API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bingai--started-p ()
  "Whether is the session started."
  (if bingai--current-session
      t
    nil))

(async-defun bingai--start ()
  "Start a new bingai session."
  (await t)
  (when bingai--current-session
    (bingai--stop bingai--current-session))
  (when-let ((conversation (await (bingai--create-conversation)))
             (session (bingai--session-new
                       :conversation conversation)))
    (setq bingai--current-session session)
    t))

(defun bingai--stop ()
  "Stop current bingai session."
  (when bingai--current-session
    (setf (bingai--session-conversation bingai--current-session) nil)
    (bingai--destroy-chathub bingai--current-session)
    (setf (bingai--session-invocation-id bingai--current-session) 0)
    (setq bingai--current-session nil)))

(async-defun bingai--say (said reply-cb)
  "Say to bingai."
  (unless bingai--current-session
    (await (bingai--start)))

  (unless (bingai--session-chathub bingai--current-session)
    (await (bingai--create-chathub bingai--current-session)))

  (await (bingai--send-said bingai--current-session said reply-cb)))

(defun bingai--replying-p ()
  "Whether bingai is currently replying."
  (if (and bingai--current-session
           (bingai--session-replying bingai--current-session))
      t
    nil))

(defun bingai--stop-replying ()
  "Stop the reply currently in progress."
  (when (bingai--replying-p)
    (setf (bingai--session-replying bingai--current-session) nil)
    (bingai--destroy-chathub bingai--current-session)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bingai msg API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bingai--msg-type-1-text (msg)
  "msg[arguments][0][messages][0][text]."
  (alist-get 'text
             (aref
              (alist-get 'messages
                         (aref (alist-get 'arguments msg) 0))
              0)))

(defun bingai--msg-type-1-message-type (msg)
  "msg[arguments][0][messages][0][messageType]."
  (alist-get 'messageType
             (aref
              (alist-get 'messages
                         (aref (alist-get 'arguments msg) 0))
              0)))

(defun bingai--msg-type-1-search-results (msg)
  "msg[arguments][0][messages][0][hiddenText]."
  (when-let ((hidden-text (alist-get 'hiddenText
                                     (aref
                                      (alist-get 'messages
                                                 (aref (alist-get 'arguments msg) 0))
                                      0))))
    (setq hidden-text (string-trim hidden-text "```json" "```"))
    (ignore-errors (alist-get 'web_search_results (json-read-from-string hidden-text)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bingai-chat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom bingai-file (expand-file-name "aichat.md" user-emacs-directory)
  "File path of save chat message."
  :group 'bingai
  :type 'string)

;;;###autoload
(defun bingai-toggle-debug ()
  "Toggle debug mode."
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

(cl-defstruct (bingai--chat
               (:constructor bingai--chat-new)
               (:copier nil))
  "A chat structure.
`buffer' is used to display chat message.
`said' is what the user said.
`replied-length' is the length of the reply.
`reply-point' is where the reply is inserted.
`search-results' is the results of web search.
`enter-src' is the mark of markdown converted to org."
  buffer
  said
  (replied-length 0)
  reply-point
  search-results
  enter-src)

(defun bingai--chat-get-buffer ()
  "Get chat buffer."
  (let ((chat-buffer (get-file-buffer bingai-file)))
    (unless chat-buffer
      (setq chat-buffer (find-file-noselect bingai-file)))
    (with-current-buffer chat-buffer
      (when (derived-mode-p 'markdown-mode)
        (unless markdown-hide-markup
          (markdown-toggle-markup-hiding))))
    chat-buffer))

(defun bingai--chat-say (chat new-p)
  "Show user said.
NEW-P is t, which means it is a new conversation."
  (with-current-buffer (bingai--chat-buffer chat)
    (goto-char (point-max))
    (let ((header-char (if (derived-mode-p 'org-mode) "*" "#")))
      (if new-p
          (insert "\n" header-char " ")
        (insert "\n" header-char header-char " ")))
    (insert (bingai--chat-said chat))
    (insert "\n\n")
    (setf (bingai--chat-reply-point chat) (point))))

(defface bingai-chat-prompt-face '((t (:height 0.8 :foreground "#006800")))
  "Face used for prompt overlay.")

(defun bingai--chat-update-prompt (buffer text)
  (with-current-buffer buffer
    (save-excursion
      (if (derived-mode-p 'org-mode)
          (org-previous-visible-heading +1)
        (markdown-previous-visible-heading +1))
      (let* ((from (line-beginning-position))
             (to (line-end-position)))
        (remove-overlays from to 'bingai--chat-handle-reply t)
        (when text
          (let ((ov (make-overlay from to)))
            (overlay-put ov 'after-string
                         (propertize
                          (concat " " text)
                          'face 'bingai-chat-prompt-face))
            (overlay-put ov 'bingai--chat-handle-reply t)))))))

(defun bingai--chat-handle-reply (msg chat)
  (let ((message-type (bingai--msg-type-1-message-type msg))
        (buffer (bingai--chat-buffer chat)))
    (pcase message-type
      ("InternalSearchQuery" (when-let ((text (bingai--msg-type-1-text msg)))
                               (bingai--chat-update-prompt buffer text)))
      ("InternalLoaderMessage" (when-let ((text (bingai--msg-type-1-text msg)))
                                 (bingai--chat-update-prompt buffer text)))
      ("InternalSearchResult" (when-let ((search-results (bingai--msg-type-1-search-results msg)))
                                (setf (bingai--chat-search-results chat) search-results)))
      (_
       (when-let* ((text (bingai--msg-type-1-text msg))
                   (replied-length (bingai--chat-replied-length chat))
                   (text-length (length text))
                   (valid (> text-length replied-length)))
         (with-current-buffer buffer
           (goto-char (bingai--chat-reply-point chat))
           (insert (substring text replied-length))

           (when (derived-mode-p 'org-mode)
             ;; convert [^1^] to [fn:1]
             (save-excursion
               (goto-char (- (bingai--chat-reply-point chat) 10))
               (when (re-search-forward "\\(\\*\\(\\*.*\\*\\)\\*\\|\\[^\\([0-9]+\\)^\\]\\|```\\([a-z]*\\)\\)" nil t)
                 (when (match-string 2)
                   (replace-match "\\2"))
                 (when (match-string 3)
                   (replace-match "[fn:\\3]"))
                 (when (match-string 4)
                   (if (bingai--chat-enter-src chat)
                       (replace-match "#+end_src")
                     (replace-match "#+begin_src \\4"))
                   (setf (bingai--chat-enter-src chat) (not (bingai--chat-enter-src chat)))))))

           (setf (bingai--chat-reply-point chat) (point)
                 (bingai--chat-replied-length chat) text-length)))))))

(defun bingai--chat-handle-reply-finished (chat)
  ;; insert search result
  (with-current-buffer (bingai--chat-buffer chat)
    (insert "\n")
    (mapc (lambda (result)
            (insert (format "%s. " (alist-get 'index result)))
            (if (derived-mode-p 'org-mode)
                (org-insert-link nil (alist-get 'url result) (alist-get 'title result))
              (insert (format "[%s](%s)" (alist-get 'title result) (alist-get 'url result))))
            (insert "\n"))
          (bingai--chat-search-results chat))
    (insert "\n")
    (bingai--chat-update-prompt (bingai--chat-buffer chat) nil))
  (message "Finished"))

(defun bingai--chat-handle-reply-error (chat msg)
  (bingai--chat-update-prompt (bingai--chat-buffer chat) nil)
  (message "%s" msg))

(defun bingai-stop-replying ()
  (interactive)
  (bingai--stop-replying))

;;;###autoload
(defun bingai-chat (said)
  (interactive "sYou say: ")
  (when (and (car current-prefix-arg)
             (= (car current-prefix-arg) 4))
    (bingai--stop))
  (if (bingai--replying-p)
      (message "Please wait for the replying finiahed before saying.")
    (let* ((chat-buffer (bingai--chat-get-buffer))
           (chat (bingai--chat-new
                  :buffer chat-buffer
                  :said said)))
      (switch-to-buffer chat-buffer)
      (bingai--chat-say chat (not (bingai--started-p)))
      (promise-then (bingai--say said (lambda (msg)
                                        (bingai--chat-handle-reply msg chat)))
                    (lambda (_)
                      (bingai--chat-handle-reply-finished chat)
                      )
                    (lambda (msg)
                      (bingai--chat-handle-reply-error chat msg))))))

(provide 'bingai)

;;; bingai.el ends here
