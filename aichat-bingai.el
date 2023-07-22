;;; aichat-bingai.el --- aichat-bingai.el   -*- lexical-binding: t; -*-

;; Filename: aichat-bingai.el
;; Description: aichat-bingai.el
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-03-11 15:12:02
;; Version: 0.1
;; Last-Updated: 2023-03-11 15:12:02
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
;; aichat-bingai.el
;;

;;; Installation:
;;
;; Put aichat-bingai.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'aichat-bingai)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET aichat-bingai RET
;;

;;; Change log:
;;
;; 2023/03/11
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
(require 'gnutls)
(require 'websocket)

(require 'aichat-util)

;;; Code:

(defgroup aichat-bingai nil
  "Bing AI in Emacs."
  :group 'aichat
  :prefix "aichat-bingai-")

(defcustom aichat-bingai-cookies-file nil
  "The path of www.bing.com cookies file.

When you set this value, bingai will login to www.bing.com through the cookies in the file."
  :group 'aichat-bingai
  :type 'string)

(defcustom aichat-bingai-conversation-style 'balanced
  "Conversation style."
  :group 'aichat-bingai
  :type '(radio
          (const :tag "More Creative" creative)
          (const :tag "More Balanced" balanced)
          (const :tag "More Precise" precise)))

(defcustom aichat-bingai-proxy nil
  "Http proxy of request bingai.

(setq aichat-bingai-proxy \"localhost:51837\")"
  :group 'aichat-bingai
  :type 'string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; websocket ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aichat-bingai--create-proxy-connection(name proxy-host proxy-port server-host server-port use-ssl)
  (let ((conn)
        (finished))
    (setq conn (make-network-process
                :name name
                :buffer nil
                :host proxy-host
                :service proxy-port
                :filter (lambda (proc data)
                          (setq finished t))))
    (process-send-string
     conn
     (format "CONNECT %s:%s HTTP/1.1\r\nHost: %s:%s\r\nProxy-Connection: Keep-Alive\r\n\r\n"
             server-host server-port server-host server-port))
    (while (not finished)
      (accept-process-output conn 0.2))
    (when use-ssl
      (setq conn (gnutls-negotiate :process conn :hostname server-host)))
    conn))

(cl-defun aichat-bingai--websocket-open (url &key protocols extensions (on-open 'identity)
                                             (on-message (lambda (_w _f))) (on-close 'identity)
                                             (on-error 'websocket-default-error-handler)
                                             (nowait nil) (custom-header-alist nil)
                                             (proxy nil))
  "Open a websocket connection to URL, returning the `websocket' struct.
The PROTOCOL argument is optional, and setting it will declare to
the server that this client supports the protocols in the list
given.  We will require that the server also has to support that
protocols.

Similar logic applies to EXTENSIONS, which is a list of conses,
the car of which is a string naming the extension, and the cdr of
which is the list of parameter strings to use for that extension.
The parameter strings are of the form \"key=value\" or \"value\".
EXTENSIONS can be NIL if none are in use.  An example value would
be (\"deflate-stream\" . (\"mux\" \"max-channels=4\")).

Cookies that are set via `url-cookie-store' will be used during
communication with the server, and cookies received from the
server will be stored in the same cookie storage that the
`url-cookie' package uses.

Optionally you can specify
ON-OPEN, ON-MESSAGE and ON-CLOSE callbacks as well.

The ON-OPEN callback is called after the connection is
established with the websocket as the only argument.  The return
value is unused.

The ON-MESSAGE callback is called after receiving a frame, and is
called with the websocket as the first argument and
`websocket-frame' struct as the second.  The return value is
unused.

The ON-CLOSE callback is called after the connection is closed, or
failed to open.  It is called with the websocket as the only
argument, and the return value is unused.

The ON-ERROR callback is called when any of the other callbacks
have an error.  It takes the websocket as the first argument, and
a symbol as the second argument either `on-open', `on-message',
or `on-close', and the error as the third argument. Do NOT
rethrow the error, or else you may miss some websocket messages.
You similarly must not generate any other errors in this method.
If you want to debug errors, set
`websocket-callback-debug-on-error' to t, but this also can be
dangerous is the debugger is quit out of.  If not specified,
`websocket-default-error-handler' is used.

For each of these event handlers, the client code can store
arbitrary data in the `client-data' slot in the returned
websocket.

The following errors might be thrown in this method or in
websocket processing, all of them having the error-condition
`websocket-error' in addition to their own symbol:

`websocket-unsupported-protocol': Data in the error signal is the
protocol that is unsupported.  For example, giving a URL starting
with http by mistake raises this error.

`websocket-wss-needs-emacs-24': Trying to connect wss protocol
using Emacs < 24 raises this error.  You can catch this error
also by `websocket-unsupported-protocol'.

`websocket-received-error-http-response': Data in the error
signal is the integer error number.

`websocket-invalid-header': Data in the error is a string
describing the invalid header received from the server.

`websocket-unparseable-frame': Data in the error is a string
describing the problem with the frame.

`nowait': If NOWAIT is true, return without waiting for the
connection to complete.

`custom-headers-alist': An alist of custom headers to pass to the
server. The car is the header name, the cdr is the header value.
These are different from the extensions because it is not related
to the websocket protocol.
"
  (let* ((name (format "websocket to %s" url))
         (url-struct (url-generic-parse-url url))
         (key (websocket-genkey))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (conn (if (member (url-type url-struct) '("ws" "wss"))
                   (let* ((type (if (equal (url-type url-struct) "ws")
                                    'plain 'tls))
                          (port (if (= 0 (url-port url-struct))
                                    (if (eq type 'tls) 443 80)
                                  (url-port url-struct)))
                          (host (url-host url-struct)))
                     (if proxy
                         (let* ((proxy-segs (split-string proxy ":"))
                                (proxy-host (nth 0 proxy-segs))
                                (proxy-port (nth 1 proxy-segs))
                                (use-ssl (eq type 'tls)))
                           (aichat-bingai--create-proxy-connection
                            name proxy-host proxy-port host port use-ssl))
                       (if (eq type 'plain)
                           (make-network-process :name name :buffer nil :host host
                                                 :service port :nowait nowait)
                         (condition-case-unless-debug nil
                             (open-network-stream name nil host port :type type :nowait nowait)
                           (wrong-number-of-arguments
                            (signal 'websocket-wss-needs-emacs-24 (list "wss")))))))
                 (signal 'websocket-unsupported-protocol (list (url-type url-struct)))))
         (websocket (websocket-inner-create
                     :conn conn
                     :url url
                     :on-open on-open
                     :on-message on-message
                     :on-close on-close
                     :on-error on-error
                     :protocols protocols
                     :extensions (mapcar 'car extensions)
                     :accept-string
                     (websocket-calculate-accept key))))
    (unless conn (error "Could not establish the websocket connection to %s" url))
    (process-put conn :websocket websocket)
    (set-process-filter conn
                        (lambda (process output)
                          (let ((websocket (process-get process :websocket)))
                            (websocket-outer-filter websocket output))))
    (set-process-sentinel
     conn
     (websocket-sentinel url conn key protocols extensions custom-header-alist nowait))
    (set-process-query-on-exit-flag conn nil)
    (websocket-ensure-handshake url conn key protocols extensions custom-header-alist nowait)
    websocket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internal ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst aichat-bingai--domain ".bing.com"
  "Bing domain for retrieve cookies.")

(defun aichat-bingai--login-p ()
  "Check if you're already login."
  (when-let* ((host-cookies
               (seq-find (lambda (host)
                           (string= (car host) aichat-bingai--domain))
                         (append url-cookie-secure-storage)))
              (user (seq-find
                     (lambda (cookie)
                       (string= (aref cookie 1) "_U"))
                     (cdr host-cookies))))
    (and user
         (not (url-cookie-expired-p user)))))

(async-defun aichat-bingai--login ()
  "Login `aichat-bingai--domain'."
  (await t)
  (unless (aichat-bingai--login-p)
    (await (aichat-refresh-cookies aichat-bingai--domain aichat-bingai-cookies-file))))

(defconst aichat-bingai--create-conversation-url "https://www.bing.com/turing/conversation/create"
  "The url of create conversation.")

(defconst aichat-bingai--conversation-headers
  `(("authority" . "edgeservices.bing.com")
    ("accept" . "application/json")
    ("accept-language" . "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6")
    ("content-type" . "application/json")
    ("referer" . "https://www.bing.com/search?q=Bing+AI&showconv=1")
    ("sec-ch-ua" . "\"Microsoft Edge\"=\"111\", \"Not(A:Brand\";v=\"8\", \"Chromium\";v=\"111\"")
    ("sec-ch-ua-mobile" . "?0")
    ("sec-ch-ua-platform" . "\"Windows\"")
    ("sec-fetch-dest" . "empty")
    ("sec-fetch-mode" . "cors")
    ("sec-fetch-site" . "same-origin")
    ("x-ms-client-request-id" . ,(aichat-uuid))
    ("x-ms-useragent" . "azsdk-js-api-client-factory/1.0.0-beta.1 core-rest-pipeline/1.10.0 OS/Win32")
    ("x-forwarded-for" . "1.1.1.1"))
  "The headers of create conversation.")

(defconst aichat-bingai--chathub-headers
  `(("User-Agent" . ,aichat-user-agent)
    ("Accept-Language" . "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6"))
  "The headers of create chathub.")

(cl-defstruct (aichat-bingai--conversation
               (:constructor aichat-bingai--conversation-new)
               (:copier nil))
  "A conversation structure.
`id', `signature' and `client-id' are obtained through the GET request `aichat-bingai--conversation-url' response."
  id
  signature
  client-id)

(async-defun aichat-bingai--create-conversation ()
  "Create a conversation through the GET request `aichat-bingai--conversation-url'."
  (await (aichat-bingai--login))
  (seq-let
      (status headers body)
      (await (aichat-http aichat-bingai--create-conversation-url
                          :proxy aichat-bingai-proxy
                          :headers aichat-bingai--conversation-headers))
    (aichat-debug "status:\n%s\nheaders:\n%s\nbody:\n%s\n" status headers body)
    (if (not (string= "200" (car status)))
        (error "Create conversation failed: %s" status)
      (if (string-empty-p body)
          (error "Your network settings are preventing access to this feature.")
        (let* ((data (aichat-json-parse body))
               (result-value (aichat-json-access data "{result}{value}")))
          (if (not (string= "Success" result-value))
              (error "Create conversation failed: %s" body)
            (aichat-bingai--conversation-new
             :id (aichat-json-access data "{conversationId}")
             :signature (aichat-json-access data "{conversationSignature}")
             :client-id (aichat-json-access data "{clientId}"))))))))

(cl-defstruct (aichat-bingai--session
               (:constructor aichat-bingai--session-new)
               (:copier nil))
  "A session structure.
`conversation' represents the `aichat-bingai--conversation'.
`chathub' represents the chathub websocket connection.
`invocation-id' indicates the number of questions.
`max-conversation' indicates the number of max conversation.
when `reset' is `t', session will be reset.
`replying' indicates whether the reply is in progress.
`buffer' saves the reply message for parsing.
`resolve' and `reject' are promise callback, call `resolve' when the reply ends
and call `reject' when error occurs.
`result' saves the result of conversation.
`err' saves the error of conversation.
Call `user-cb' when a message arrives."
  conversation
  chathub
  (invocation-id 0)
  (max-conversation 10)
  (reset nil)
  (replying nil)
  (buffer "")
  resolve
  reject
  (result nil)
  (err nil)
  (expiry-time nil)
  user-cb)

(defconst aichat-bingai--chathub-message-delimiter (char-to-string #x1e)
  "Websocket json message delimiter.")

(defun aichat-bingai--chathub-send-heart (ws)
  "Send type 6 message."
  (websocket-send-text ws (concat (aichat-json-serialize (list :type 6))
                                  aichat-bingai--chathub-message-delimiter)))

(defun aichat-bingai--chathub-reply-finished (session &optional error reset)
  "Handler of reply finished."
  (setf (aichat-bingai--session-replying session) nil)
  (setf (aichat-bingai--session-err session) error)
  (setf (aichat-bingai--session-reset session) reset)
  (when reset
    (websocket-close (aichat-bingai--session-chathub session)))
  (let ((err (aichat-bingai--session-err session)))
    (if err
        (when-let ((reject-func (aichat-bingai--session-reject session)))
          (funcall reject-func err))
      (when-let ((resolve-func (aichat-bingai--session-resolve session)))
        (funcall  resolve-func (aichat-bingai--session-result session))))))

(defun aichat-bingai--chathub-parse-message (session text)
  "Parse chathub websocket json message."
  (aichat-debug "Recv text:\n%s" text)
  (let ((buffer (concat (aichat-bingai--session-buffer session) text))
        (start-pos 0)
        (match-pos nil)
        (object))
    (aichat-debug "buffer:\n%s" buffer)
    (catch 'not-find
      (while t
        (setq match-pos (string-match-p aichat-bingai--chathub-message-delimiter buffer start-pos))
        (if (not match-pos)
            (throw 'not-find match-pos)
          (setq object (aichat-json-parse (substring buffer start-pos match-pos)))
          (aichat-debug "object:\n%s" object)
          (pcase (aichat-json-access object "{type}")
            (1 (let* ((user-cb (aichat-bingai--session-user-cb session))
                      (messages (aichat-json-access object "{arguments}[0]{messages}"))
                      (throttling (aichat-json-access object "{arguments}[0]{throttling}"))
                      (message-type (if messages (aichat-json-access messages "[0]{messageType}") nil)))
                 (cond
                  (throttling
                   (setf (aichat-bingai--session-max-conversation session)
                         (aichat-json-access throttling "{maxNumUserMessagesInConversation}")))
                  ((string= message-type "Disengaged")
                   (aichat-bingai--chathub-reply-finished session "Conversation disengaged, next conversation will be restarted." t))
                  (t
                   (when user-cb
                     (condition-case error
                         (funcall user-cb object)
                       (error
                        (aichat-bingai--chathub-reply-finished session (format "User callback error: %s" error)))))))))
            (2  (let* ((result (aichat-json-access object "{item}{result}"))
                       (value (aichat-json-access result "{value}"))
                       (message (aichat-json-access result "{message}")))
                  (if (string= "Success" value)
                      (progn
                        (when-let* ((expiry-time-str
                                     (ignore-errors
                                       (aichat-json-access object "{item}{conversationExpiryTime}")))
                                    (expiry-time (date-to-time expiry-time-str)))
                          (setf (aichat-bingai--session-expiry-time session) expiry-time))
                        (setf (aichat-bingai--session-result session) object))
                    (aichat-bingai--chathub-reply-finished session (format "%s:%s\n" value message)
                                                           (string= "InvalidSession" value)))))
            (3 (let ((err (aichat-json-access object "{error}")))
                 (aichat-bingai--chathub-reply-finished session err)))
            (6 (when-let ((chathub (aichat-bingai--session-chathub session))
                          (replying (aichat-bingai--session-replying session)))
                 (aichat-bingai--chathub-send-heart chathub))))
          (setq start-pos (1+ match-pos)))))
    (setf (aichat-bingai--session-buffer session) (substring buffer start-pos))))

(defconst aichat-bingai--chathub-url "wss://sydney.bing.com/sydney/ChatHub"
  "The url of create chathub.")

(defun aichat-bingai--create-chathub (session)
  "Create a websocket connection to `aichat-bingai--chathub-url'.

Call resolve when the handshake with chathub passed."
  (promise-new
   (lambda (resolve reject)
     (aichat-bingai--websocket-open
      aichat-bingai--chathub-url
      :proxy aichat-bingai-proxy
      :custom-header-alist aichat-bingai--chathub-headers
      :on-open (lambda (ws)
                 (aichat-debug "====== chathub opened ======")
                 ;; send handshake
                 (if (and ws (websocket-openp ws))
                     (websocket-send-text ws (concat (aichat-json-serialize
                                                      (list :protocol "json" :version 1))
                                                     aichat-bingai--chathub-message-delimiter))
                   (funcall reject "Chathub unexpected closed during handshake.")))
      :on-close (lambda (_ws)
                  (aichat-debug "====== chathub closed ======")
                  (if (not (aichat-bingai--session-chathub session))
                      (funcall reject "Chathub unexpected closed during handshake.")
                    (setf (aichat-bingai--session-chathub session) nil)
                    (when (aichat-bingai--session-replying session)
                      ;; close when replying
                      (setf (aichat-bingai--session-replying session) nil)
                      (when-let ((reject-func (aichat-bingai--session-reject session)))
                        (funcall  reject-func "Chathub closed unexpectedly during reply.")))))
      :on-message (lambda (ws frame)
                    (let ((text (websocket-frame-text frame)))
                      (condition-case error
                          (progn
                            (aichat-debug "Receive handshake response: %s" text)
                            (aichat-json-parse (car (split-string text aichat-bingai--chathub-message-delimiter)))
                            (aichat-bingai--chathub-send-heart ws)
                            (setf (websocket-on-message ws)
                                  (lambda (_ws frame)
                                    (aichat-bingai--chathub-parse-message session (websocket-frame-text frame))))
                            (setf (aichat-bingai--session-chathub session) ws)
                            (funcall resolve t))
                        (error (funcall reject error)))))))))

(defun aichat-bingai--close-chathub (session)
  "Close chathub websocket connection."
  (when-let ((chathub (aichat-bingai--session-chathub session)))
    (when (websocket-openp chathub)
      (websocket-close chathub))))

(defun aichat-bingai--reply-options (style)
  (apply
   #'vector
   "nlu_direct_response_filter"
   "deepleo"
   "disable_emoji_spoken_text"
   "responsible_ai_policy_2235"
   "enablemm"
   "iycapbing"
   "iyxapbing"
   "refpromptv1"
   "enuaug"
   "dv3sugg"
   "autosave"
   "iyoloxap"
   "iyoloneutral"
   (pcase style
     ('creative (list "h3imaginative" "clgalileo" "gencontentv3"))
     ('balanced (list "galileo" "saharagenconv5"))
     ('precise (list "h3precise" "clgalileo" "gencontentv3")))))

(defconst aichat-bingai--allowed-message-types
  [
   "Chat"
   "InternalSearchQuery"
   "InternalSearchResult"
   "InternalLoaderMessage"
   "RenderCardRequest"
   "AdsQuery"
   "SemanticSerp"
   "GenerateContentQuery"
   "SearchQuery"
   ])

(defconst aichat-bingai--slice-ids
  [
   "winmuid3tf"
   "anssupltmrd1"
   "imgchatgptv2"
   "bingsr3-v1"
   "revpayaad"
   "winstmsg2tf"
   "602refusal"
   "621alllocs0"
   "621docxfmtho"
   "330uaug"
   "0626snptrcs0"
   "424dagslnv1s0"])

(defun aichat-bingai--make-request (session text style allowed-message-types options-sets)
  (unless allowed-message-types
    (setq allowed-message-types aichat-bingai--allowed-message-types))

  (unless options-sets
    (setq options-sets (aichat-bingai--reply-options style)))

  (let* ((conversation (aichat-bingai--session-conversation session))
         (invocation-id (aichat-bingai--session-invocation-id session))
         (request (list :arguments
                        (vector
                         (list :source "cib"
                               :optionsSets options-sets
                               :allowedMessageTypes (vconcat allowed-message-types ["Disengaged"])
                               :sliceIds aichat-bingai--slice-ids
                               :spokenTextMode: "None"
                               :isStartOfSession (if (= 0 invocation-id)
                                                     t
                                                   :json-false)
                               :message (list :author "user"
                                              :inputMethod "Keyboard"
                                              :text text
                                              :messageType "Chat")
                               :conversationSignature (aichat-bingai--conversation-signature conversation)
                               :participant (list :id (aichat-bingai--conversation-client-id conversation))
                               :conversationId (aichat-bingai--conversation-id conversation)))
                        :invocationId (number-to-string (aichat-bingai--session-invocation-id session))
                        :target "chat"
                        :type 4)))
    (concat (aichat-json-serialize request)  aichat-bingai--chathub-message-delimiter)))

(defvar aichat-bingai--current-session nil  ;; only one session
  "Bingai session.")

(defun aichat-bingai--get-current-session ()
  "Return current session."
  aichat-bingai--current-session)

(defun aichat-bingai--set-current-session (session)
  "Set current session."
  (setq aichat-bingai--current-session session))

(defun aichat-bingai--remove-current-session ()
  "Remove current session."
  (setq aichat-bingai--current-session nil))

(defun aichat-bingai--stop-session ()
  "Stop current bingai session."
  (when-let ((session (aichat-bingai--get-current-session)))
    (setf (aichat-bingai--session-conversation session) nil)
    (aichat-bingai--close-chathub session)
    (aichat-bingai--remove-current-session)))

(async-defun aichat-bingai--start-session ()
  "Start a new aichat-bingai session."
  (await t)
  (aichat-bingai--stop-session)
  (when-let ((conversation (await (aichat-bingai--create-conversation)))
             (session (aichat-bingai--session-new
                       :conversation conversation)))
    (aichat-bingai--set-current-session session)
    t))

(defun aichat-bingai--ensure-conversation-valid ()
  (when-let* ((session (aichat-bingai--get-current-session))
              (invocation-id (aichat-bingai--session-invocation-id session))
              (max-conversation (aichat-bingai--session-max-conversation session)))
    (when-let* ((expiry-time (aichat-bingai--session-expiry-time session))
                (expiry-p (ignore-errors
	                        (time-less-p expiry-time nil))))
      (aichat-bingai--stop-session))
    (when (aichat-bingai--session-reset session)
      (aichat-bingai--stop-session))
    (when (>= invocation-id max-conversation)
      (aichat-bingai--stop-session))))

(async-defun aichat-bingai--send-request (text style allowed-message-types options-sets &optional callback)
  (aichat-bingai--ensure-conversation-valid)
  (let ((session (aichat-bingai--get-current-session)))
    (unless session
      (await (aichat-bingai--start-session))
      (setq session (aichat-bingai--get-current-session)))
    (unless (aichat-bingai--session-chathub session)
      (await (aichat-bingai--create-chathub session)))

    (await (promise-new
            (lambda (resolve reject)
              (let ((request (aichat-bingai--make-request session text style allowed-message-types options-sets)))
                (aichat-debug "Send request:\n%s\n" request)
                (websocket-send-text (aichat-bingai--session-chathub session) request)
                (setf (aichat-bingai--session-invocation-id session) (1+ (aichat-bingai--session-invocation-id session))
                      (aichat-bingai--session-replying session) t
                      (aichat-bingai--session-buffer session) ""
                      (aichat-bingai--session-resolve session) resolve
                      (aichat-bingai--session-reject session) reject
                      (aichat-bingai--session-result session) nil
                      (aichat-bingai--session-err session) nil
                      (aichat-bingai--session-user-cb session) callback)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Image  API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(async-defun aichat-bingai--image-download (urls resolve reject)
  (await nil)
  (condition-case error
      (let ((paths))
        (dolist (url urls)
          (when-let* ((name (concat (car (last (split-string (car (split-string url "?")) "/"))) ".jpeg"))
                      (filepath (expand-file-name name (temporary-file-directory)))
                      (output (await (aichat-shell-command (format "%s --silent \"%s\" --output %s" aichat-curl-program url filepath)))))
            (push (cons url filepath) paths)))
        (funcall resolve paths))
    (error (funcall reject error))))

(defun aichat-bingai--image-polling (request-id prompt resolve reject)
  (let ((polling-url (format "https://www.bing.com/images/create/async/results/%s" request-id)))
    (promise-then (aichat-http polling-url
                               :proxy aichat-bingai-proxy
                               :params `(("q" . ,prompt)))
                  (lambda (response)
                    (if (not (string= "200" (caar response)))
                        (progn
                          (funcall reject (format "Polling image error: %s" response)))
                      (let ((body (car (last response)))
                            (urls (list)))
                        (if (string-empty-p body)
                            (run-at-time 5 nil #'aichat-bingai--image-polling request-id prompt resolve reject)
                          (with-temp-buffer
                            (insert body)
                            (goto-char (point-min))
                            (while (re-search-forward (rx "src=\""
                                                          (group (1+ (not "\"")))
                                                          "\"")
                                                      nil t)
                              (push (match-string 1) urls))
                            (aichat-bingai--image-download urls resolve reject))))))
                  (lambda (err)
                    (message "Polling error but continue: %s" err)
                    (run-at-time 5 nil #'aichat-bingai--image-polling request-id prompt resolve reject)))))

(async-defun aichat-bingai--image-create (prompt)
  "Create images with PROMPT."
  (let ((iframeid (aichat-uuid))
        (response)
        (location)
        (redirect-url)
        (request-id)
        (polling-url))
    (setq response (await (aichat-http "https://www.bing.com/images/create"
                                       :proxy aichat-bingai-proxy
                                       :type "POST"
                                       :params `(("q" . ,prompt)
                                                 ("rt" . "3")
                                                 ("FORM" . "GENCRE"))
                                       :data "")))
    (if (not (string= "302" (caar response)))
        (error (format "Image create: %s" response))
      (setq location (aichat-read-header-value "Location" (cadr response)))
      (setq redirect-url (concat "https://www.bing.com" (replace-regexp-in-string "&nfy=1" "" location)))
      (setq request-id (cadr (split-string redirect-url "id=")))
      (setq response (await (aichat-http redirect-url
                                         :proxy aichat-bingai-proxy)))
      (await
       (promise-new
        (lambda (resolve reject)
          (aichat-bingai--image-polling request-id prompt resolve reject)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bingai API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aichat-bingai-conversationing-p ()
  "Whether conversation or not."
  (when-let ((session (aichat-bingai--get-current-session)))
    (aichat-bingai--session-replying session)))

(defun aichat-bingai-conversation-start-p ()
  "Whether is start of conversation."
  (aichat-bingai--ensure-conversation-valid)
  (not (aichat-bingai--get-current-session)))

(defun aichat-bingai-conversation-reset ()
  "Reset conversation."
  (aichat-bingai--stop-session))

(cl-defun aichat-bingai-conversation (text &rest settings
                                           &key
                                           (style nil)
                                           (allowed-message-types nil)
                                           (options-sets nil)
                                           (on-success nil)
                                           (on-error nil))
  "Send a chat TEXT to Bing.

`style' is the conversation style, look `aichat-bingai-conversation-stye' for detail.
`allowed-message-types' is the message type allowed to return,
all types in `aichat-bingai--allowed-message-types'."
  (when (aichat-bingai-conversationing-p)
    (error "Please wait for the conversation finished before call."))
  (unless style
    (setq style aichat-bingai-conversation-style))
  (unless allowed-message-types
    (setq allowed-message-types (vector "Chat")))

  (promise-then (aichat-bingai--send-request text style allowed-message-types options-sets)
                (lambda (result)
                  (when on-success
                    (funcall on-success result)))
                (lambda (err)
                  (when on-error
                    (funcall on-error err)))))


(cl-defun aichat-bingai-conversation-stream (text callback &rest settings
                                                  &key
                                                  (style nil)
                                                  (allowed-message-types nil)
                                                  (options-sets nil)
                                                  (on-success nil)
                                                  (on-error nil))
  "Send a chat TEXT to Bing.

`style' is the conversation style, look `aichat-bingai-conversation-stye' for detail.
`allowed-message-types' is the message type allowed to return,
all types in `aichat-bingai--allowed-message-types'."
  (when (aichat-bingai-conversationing-p)
    (error "Please wait for the conversation finished before call."))

  (unless style
    (setq style aichat-bingai-conversation-style))

  (unless allowed-message-types
    (setq allowed-message-types (vector "Chat")))

  (promise-then (aichat-bingai--send-request text style allowed-message-types options-sets
                                             (lambda (message)
                                               (when callback
                                                 (funcall callback message))))
                (lambda (result)
                  (when on-success
                    (funcall on-success result)))
                (lambda (err)
                  (when on-error
                    (funcall on-error err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Messages API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aichat-bingai-message-type-1-text (message)
  "message[arguments][0][messages][0][text]."
  (aichat-json-access message "{arguments}[0]{messages}[0]{text}"))

(defun aichat-bingai-message-type-1-search-result (message)
  "message[arguments][0][messages][0][hiddenText]."
  (when-let* ((hidden-text (aichat-json-access message "{arguments}[0]{messages}[0]{hiddenText}"))
              (hidden-object (ignore-errors  (aichat-json-parse (string-trim hidden-text "```json" "```")))))
    (cl-loop for result in hidden-object
             vconcat (cdr result))))

(defun aichat-bingai-message-type-1-message-type (message)
  "msg[arguments][0][messages][0][messageType]."
  (aichat-json-access message "{arguments}[0]{messages}[0]{messageType}"))

(defun aichat-bingai-message-type-1-suggestion (message)
  "message[arguments][0][messages][0][suggestedResponses]."
  (when-let ((suggested-responses (aichat-json-access message "{arguments}[0]{messages}[0]{suggestedResponses}")))
    (cl-loop for suggested-response across suggested-responses
             collect (aichat-json-access suggested-response "{text}"))))

(defun aichat-bingai-message-type-2-text (message)
  "message[item][messages][?][text]."
  (when-let ((messages (aichat-json-access message "{item}{messages}")))
    (cl-loop for msg across messages
             do (let ((msg-type (aichat-json-access msg "{messageType}"))
                      (author (aichat-json-access msg "{author}")))
                  (when (and (not msg-type) (string= author "bot"))
                    (cl-return (aichat-json-access msg "{text}")))))))

(defun aichat-bingai-message-type-2-search-result (message)
  "message[arguments][0][messages][?][hiddenText]."
  (when-let ((messages  (aichat-json-access message "{item}{messages}")))
    (cl-loop for msg across messages
             do (let ((msg-type (aichat-json-access msg "{messageType}"))
                      (author (aichat-json-access msg "{author}")))
                  (when (and (string= msg-type "InternalSearchResult") (string= author "bot"))
                    (when-let* ((hidden-text (aichat-json-access msg "{hiddenText}"))
                                (hidden-object (ignore-errors (aichat-json-parse (string-trim hidden-text "```json" "```")))))
                      (cl-return
                       (cl-loop for result in hidden-object
                                vconcat (cdr result)))))))))

(defun aichat-bingai-message-type-2-suggestion (message)
  "message[item][messages][?][text]."
  (when-let ((messages (aichat-json-access message "{item}{messages}")))
    (cl-loop for msg across messages
             do (let ((msg-type (aichat-json-access msg "{messageType}"))
                      (author (aichat-json-access msg "{author}")))
                  (when (and (not msg-type) (string= author "bot"))
                    (cl-return
                     (when-let ((suggested-responses (aichat-json-access msg "{suggestedResponses}")))
                       (cl-loop for suggested-response across suggested-responses
                                collect (aichat-json-access suggested-response "{text}")))))))))

(defun aichat-bingai-message-type-2-image-prompt (message)
  "message[arguments][0][messages][?][text]."
  (when-let ((messages  (aichat-json-access message "{item}{messages}")))
    (cl-loop for msg across messages
             do (let ((msg-type (aichat-json-access msg "{messageType}"))
                      (author (aichat-json-access msg "{author}")))
                  (when (and (string= msg-type "GenerateContentQuery") (string= author "bot"))
                    (when-let ((type (aichat-json-access msg "{contentType}"))
                               (text (aichat-json-access msg "{text}")))
                      (if (string= "IMAGE" type)
                          (cl-return text)
                        (cl-return nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Chat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom aichat-bingai-chat-file (expand-file-name "aichat.md" user-emacs-directory)
  "File path of save chat message."
  :group 'aichat-bingai
  :type 'string)

(defcustom aichat-bingai-chat-display-function 'display-buffer
  "The function of how to display `aichat-bingai-chat-file' buffer."
  :group 'aichat-bingai
  :type 'symbol)

(defface aichat-bingai-chat-prompt-face '((t (:height 0.8 :foreground "#006800")))
  "Face used for prompt overlay.")

(cl-defstruct (aichat-bingai--chat
               (:constructor aichat-bingai--chat-new)
               (:copier nil))
  "A chat structure.
`buffer' is used to display chat message.
`said' is what the user said.
`replied-length' is the length of the reply.
`reply-point' is where the reply is inserted."
  buffer
  said
  (replied-length 0)
  reply-point)

(defvar aichat-bingai--chat-suggestion nil)

(defun aichat-bingai--chat-get-buffer ()
  "Get chat buffer."
  (let ((chat-buffer (get-file-buffer aichat-bingai-chat-file)))
    (unless chat-buffer
      (setq chat-buffer (find-file-noselect aichat-bingai-chat-file)))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (when (derived-mode-p 'markdown-mode)
        (unless markdown-hide-markup
          (markdown-toggle-markup-hiding)))
      (when (and (featurep 'pangu-spacing) pangu-spacing-mode)
        (pangu-spacing-mode -1)))
    chat-buffer))

(defun aichat-bingai--chat-say (chat new-p)
  "Show user said.
NEW-P is t, which means it is a new conversation."
  (with-current-buffer (aichat-bingai--chat-buffer chat)
    (goto-char (point-max))
    (let ((header-char (if (derived-mode-p 'org-mode) "*" "#")))
      (if new-p
          (insert "\n" header-char " ")
        (insert "\n" header-char header-char " ")))
    (insert (aichat-bingai--chat-said chat))
    (insert "\n\n")
    (setf (aichat-bingai--chat-reply-point chat) (point))
    (aichat-bingai--chat-update-prompt chat "Replying...")))

(defun aichat-bingai--chat-update-prompt (chat text)
  (with-current-buffer (aichat-bingai--chat-buffer chat)
    (save-mark-and-excursion
      (goto-char (aichat-bingai--chat-reply-point chat))
      (if (derived-mode-p 'org-mode)
          (org-previous-visible-heading +1)
        (markdown-previous-visible-heading +1))
      (let* ((from (line-beginning-position))
             (to (line-end-position)))
        (remove-overlays from to 'aichat-bingai--chat-handle-reply t)
        (when text
          (let ((ov (make-overlay from to)))
            (overlay-put ov 'after-string
                         (propertize
                          (concat " " text)
                          'face 'aichat-bingai-chat-prompt-face))
            (overlay-put ov 'aichat-bingai--chat-handle-reply t)))))))

(defun aichat-bingai--chat-handle-reply (msg chat)
  (let ((message-type (aichat-bingai-message-type-1-message-type msg))
        (buffer (aichat-bingai--chat-buffer chat)))
    (pcase message-type
      ("InternalSearchQuery" (when-let ((text (aichat-bingai-message-type-1-text msg)))
                               (aichat-bingai--chat-update-prompt chat text)))
      ("InternalLoaderMessage" (when-let ((text (aichat-bingai-message-type-1-text msg)))
                                 (aichat-bingai--chat-update-prompt chat text)))
      (_
       (when-let* ((text (aichat-bingai-message-type-1-text msg))
                   (replied-length (aichat-bingai--chat-replied-length chat))
                   (text-length (length text))
                   (valid (> text-length replied-length)))
         (with-current-buffer buffer
           (save-mark-and-excursion
             (goto-char (aichat-bingai--chat-reply-point chat))
             (insert (substring text replied-length))
             (setf (aichat-bingai--chat-reply-point chat) (point)
                   (aichat-bingai--chat-replied-length chat) text-length))))))))

(defun aichat-bingai--chat-convert-to-org ()
  (org-previous-visible-heading +1)
  (while (re-search-forward "\\(\\*\\(\\*.*\\*\\)\\*\\|\\[^\\([0-9]+\\)^\\]\\|`\\([^`]+\\)`\\|```\\([a-z]*\\(.\\|\n\\)*\\)```\\)" nil t)
    (when (match-string 2)
      (replace-match "\\2"))
    (when (match-string 3)
      (replace-match "[fn:\\3]"))
    (when (match-string 4)
      (replace-match "=\\4="))
    (when (match-string 5)
      (replace-match "#+begin_src \\5#+end_src"))))

(defun aichat-bingai--chat-handle-reply-finished (chat msg)
  ;; update suggestion
  (setq aichat-bingai--chat-suggestion (aichat-bingai-message-type-2-suggestion msg))

  ;; convert to org
  (with-current-buffer (aichat-bingai--chat-buffer chat)
    (when (derived-mode-p 'org-mode)
      (save-mark-and-excursion
        (aichat-bingai--chat-convert-to-org))))

  ;; insert search result
  (when-let ((search-results (aichat-bingai-message-type-2-search-result msg)))
    (with-current-buffer (aichat-bingai--chat-buffer chat)
      (save-mark-and-excursion
        (goto-char (aichat-bingai--chat-reply-point chat))
        (end-of-line)
        (insert "\n")
        (mapc (lambda (result)
                (aichat-debug "Insert search result: %s"  result)
                (let ((index (aichat-json-access result "{index}"))
                      (title (or (aichat-json-access result "{title}")
                                 (chat-json-access result "{data}{Title}")))
                      (url (aichat-json-access result "{url}")))
                  (insert (format "%s. " index))
                  (if (derived-mode-p 'org-mode)
                      (org-insert-link nil url (or title url))
                    (insert (format "[%s](%s)" (or title url) url)))
                  (insert "\n")))
              search-results)
        (insert "\n"))))

  ;; generage image
  (let ((image-prompt (aichat-bingai-message-type-2-image-prompt msg)))
    (if (not image-prompt)
        (aichat-bingai--chat-update-prompt chat nil)
      (aichat-bingai--chat-update-prompt chat "Generate image...")
      (promise-then (aichat-bingai--image-create image-prompt)
                    (lambda (paths)
                      (aichat-bingai--chat-update-prompt chat nil)
                      (when paths
                        (with-current-buffer (aichat-bingai--chat-buffer chat)
                          (goto-char (aichat-bingai--chat-reply-point chat))
                          (save-mark-and-excursion
                            (mapc (lambda (path)
                                    (if (derived-mode-p 'org-mode)
                                        (insert (format "\n[[file:%s]] \n" (cdr path)))
                                      (insert (format "\n![%s](%s) \n" (car path) (cdr path)))))
                                  paths))
                          (if (derived-mode-p 'org-mode)
                              (org-display-inline-images)
                            (markdown-display-inline-images)))))
                    (lambda (err)
                      (message "Image create error: %s" err)
                      (aichat-bingai--chat-update-prompt chat nil))))))

(defun aichat-bingai--chat-handle-reply-error (chat msg)
  (aichat-bingai--chat-update-prompt chat nil)
  (message "%s" msg))

;;;###autoload
(defun aichat-bingai-chat (said &optional style)
  "Chat with Bing AI."
  (interactive (list (completing-read "You say: " aichat-bingai--chat-suggestion nil nil)))
  (when (and (car current-prefix-arg)
             (= (car current-prefix-arg) 4))
    (aichat-bingai-conversation-reset))

  (if (aichat-bingai-conversationing-p)
      (message "Please wait for the conversation finished before saying.")
    (let* ((chat-buffer (aichat-bingai--chat-get-buffer))
           (chat (aichat-bingai--chat-new
                  :buffer chat-buffer
                  :said said)))
      (if (and aichat-bingai-chat-display-function (functionp aichat-bingai-chat-display-function))
          (funcall aichat-bingai-chat-display-function chat-buffer)
        (switch-to-buffer chat-buffer))

      (aichat-bingai--chat-say chat (aichat-bingai-conversation-start-p))

      (aichat-bingai-conversation-stream said (lambda (msg)
                                                (aichat-bingai--chat-handle-reply msg chat))
                                         :style style
                                         :allowed-message-types ["Chat"
                                                                 "InternalSearchQuery"
                                                                 "InternalSearchResult"
                                                                 "InternalLoaderMessage"
                                                                 "GenerateContentQuery"]
                                         :on-success (lambda (msg)
                                                       (aichat-bingai--chat-handle-reply-finished chat msg))
                                         :on-error (lambda (msg)
                                                     (aichat-bingai--chat-handle-reply-error chat msg))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Assistant ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom aichat-bingai-assistant-buffer "*Aichat-BingAI-Assistant*"
  "The buffer of show assistant message."
  :group 'aichat-bingai
  :type 'string)

(defcustom aichat-bingai-assistant-display-function 'display-buffer
  "The function of display `aichat-bingai-assistant-buffer'."
  :group 'aichat-bingai
  :type 'symbol)

(defun aichat-bingai-assistant-get-buffer ()
  (get-buffer-create aichat-bingai-assistant-buffer))

(defun aichat-bingai-assistant (text &optional style)
  "Send the region or input to Bing and display the returned result to `aichat-bingai-assistant-buffer'."
  (interactive (list (aichat-read-region-or-input "Input text: ")))
  (when (and text (not (string-empty-p text)))
    (aichat-bingai-conversation text
                                :style style
                                :allowed-message-types ["Chat"
                                                        "GenerateContentQuery"]
                                :on-success (lambda (msg)
                                              (when-let ((content (aichat-bingai-message-type-2-text msg))
                                                         (buffer (aichat-bingai-assistant-get-buffer)))
                                                (with-current-buffer buffer
                                                  (goto-char (point-max))
                                                  (insert content)
                                                  (insert "\n\n"))
                                                (let ((image-prompt (aichat-bingai-message-type-2-image-prompt msg)))
                                                  (if (not image-prompt)
                                                      (funcall aichat-bingai-assistant-display-function buffer)
                                                    (promise-then (aichat-bingai--image-create image-prompt)
                                                                  (lambda (paths)
                                                                    (when paths
                                                                      (with-current-buffer buffer
                                                                        (markdown-mode)
                                                                        (goto-char (point-max))
                                                                        (save-mark-and-excursion
                                                                          (mapc (lambda (path)
                                                                                  (if (derived-mode-p 'org-mode)
                                                                                      (insert (format "\n[[file:%s]] \n" (cdr path)))
                                                                                    (insert (format "\n![%s](%s) \n" (car path) (cdr path)))))
                                                                                paths))
                                                                        (if (derived-mode-p 'org-mode)
                                                                            (org-display-inline-images)
                                                                          (markdown-display-inline-images))))
                                                                    (funcall aichat-bingai-assistant-display-function buffer))
                                                                  (lambda (err)
                                                                    (message "Image create error: %s" err)
                                                                    (funcall aichat-bingai-assistant-display-function buffer)))))))
                                :on-error (lambda (err)
                                            (message "Error: %s" err)))))

(defun aichat-bingai-replace-or-insert (text &optional style)
  "Send the region or input to Bing and replace the selected region or insert at the current position with the returned result."
  (interactive (list (aichat-read-region-or-input "Input text: ")))
  (when (and text (not (string-empty-p text)))
    (let* ((cur-buf (current-buffer))
           (cur-pos (with-current-buffer cur-buf (point)))
           (reg-beg (when (use-region-p) (region-beginning)))
           (reg-end (when (use-region-p) (region-end))))
      (aichat-bingai-conversation text
                                  :style style
                                  :on-success (lambda (msg)
                                                (when-let ((content (aichat-bingai-message-type-2-text msg)))
                                                  (with-current-buffer cur-buf
                                                    (if (and reg-beg reg-end)
                                                        (replace-region-contents reg-beg reg-end (lambda () content))
                                                      (goto-char cur-pos)
                                                      (insert content)))))
                                  :on-error (lambda (err)
                                              (message "Error: %s" err))))))

(cl-defmacro aichat-bingai-prompt-create (name &rest args
                                               &key
                                               (input-prompt "Input text: ")
                                               (text-format "%s")
                                               (style nil)
                                               (chat nil)
                                               (assistant nil)
                                               (replace-or-insert nil))
  "This macro will generate three functions: aichat-bingai-chat-name, aichat-bingai-assistant-name or aichat-bingai-replace-or-insert-name.

INPUT-PROMPT: The prompt before the user input in minibuffer.
TEXT-FORMAT: Formating string, %s is replaced by what the user input."
  (let ((chat-func (intern (format "aichat-bingai-chat-%s" name)))
        (assistant-func (intern (format "aichat-bingai-assistant-%s" name)))
        (replace-func (intern (format "aichat-bingai-replace-or-insert-%s" name))))
    `(progn
       (when ,chat
         (defun ,chat-func(text)
           (interactive (list (aichat-read-region-or-input ,input-prompt)))
           (when text
             (let ((query (format ,text-format text)))
               (aichat-bingai-chat query ,style)))))
       (when ,assistant
         (defun ,assistant-func(text)
           (interactive (list (aichat-read-region-or-input ,input-prompt)))
           (when text
             (let ((query (format ,text-format text)))
               (aichat-bingai-assistant query ,style)))))
       (when ,replace-or-insert
         (defun ,replace-func(text)
           (interactive (list (aichat-read-region-or-input ,input-prompt)))
           (when text
             (let ((query (format ,text-format text)))
               (aichat-bingai-replace-or-insert query ,style))))))))

(provide 'aichat-bingai)

;;; aichat-bingai.el ends here
