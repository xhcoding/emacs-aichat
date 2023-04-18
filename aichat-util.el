;;; aichat-util.el --- aichat-util   -*- lexical-binding: t; -*-

;; Filename: aichat-util.el
;; Description: aichat-util
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-03-04 22:04:05
;; Version: 0.1
;; Last-Updated: 2023-03-04 22:04:05
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
;; aichat-util
;;

;;; Installation:
;;
;; Put aichat-util.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'aichat-util)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET aichat-util RET
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
(eval-when-compile (require 'cl-lib))

(require 'rx)
(require 'url)
(require 'url-http)
(require 'json)
(require 'seq)

(require 'async-await)

;; setup url library, avoid set cookie failed
(url-do-setup)

;;; Code:

(defcustom aichat-debug nil
  "When set to `t', it will output more debug message in the *AICHAT-DEBUG* buffer."
  :group 'aichat
  :type 'boolean)

(defcustom aichat-user-agent "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Mobile Safari/537.36 Edg/111.0.1661.41"
  "`aichat-user-agent' is used to set the value of User-Agent."
  :group 'aichat
  :type 'string)

(defcustom aichat-http-backend 'curl
  "Http backend, curl or url."
  :group 'aichat
  :type '(radio
          (const :tag "curl" curl)
          (const :tag "url" url)))

;;;###autoload
(defun aichat-toggle-debug ()
  "Toggle debug mode."
  (interactive)
  (cond
   (aichat-debug
    (setq aichat-debug nil
          url-debug nil
          websocket-debug nil)
    (message "Turn off aichat debug mode."))
   (t
    (setq aichat-debug t
          url-debug t
          websocket-debug t)
    (message "Turn on aichat debug mode"))))

(defun aichat-debug (str &rest args)
  "Print debug message to *AICHAT-DEBUG* buffer when `aichat-debug' is set `t'"
  (when aichat-debug
    (with-current-buffer (get-buffer-create "*AICHAT-DEBUG*")
      (goto-char (point-max))
      (insert (apply #'format str args) "\n"))))

(defun aichat-uuid ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aichat-read-region-or-input (input-prompt)
  "Read string from region or input."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (read-string input-prompt)))

(defun aichat-read-header-value (header-key headers-alist)
  "Read header value with HEADER-KEY in HEADERS-ALIST."
  (alist-get header-key headers-alist nil nil '(lambda (o1 o2)
                                                 (string= (downcase o1) (downcase o2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JSON utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro aichat-json-serialize (params)
  "Serialize object to json string."
  (if (progn
        (require 'json)
        (and (fboundp 'json-serialize)
             (> emacs-major-version 27)))
      `(json-serialize ,params
                       :null-object nil
                       :false-object :json-false)
    `(let ((json-false :json-false))
       (json-encode ,params))))

(defmacro aichat-json-parse (str)
  "Parse json string STR."
  (if (progn
        (require 'json)
        (fboundp 'json-parse-string))
      `(json-parse-string ,str
                          :object-type 'alist
                          :null-object nil
                          :false-object nil)
    `(let ((json-array-type 'vector)
           (json-object-type 'alist)
           (json-false nil))
       (json-read-from-string ,str))))

(defun aichat-json-parse-file (file)
  "Read the JSON object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (aichat-json-parse (buffer-string))))

(defmacro aichat-json-access (object str)
  "Access json element with {object}[array-index]."
  (let ((res object)
        (start-pos 0))
    (cl-loop for index from 0 to (1- (length str))
             do (let ((ch (aref str index)))
                  (cond
                   ((or (= ch 123) (= ch 91))
                    (setq start-pos index))
                   ((= ch 125)
                    (setq res `(alist-get ',(intern (substring str (1+ start-pos) index)) ,res)))
                   ((= ch 93)
                    (setq res `(aref ,res ,(string-to-number (substring str (1+ start-pos) index))))))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTTP utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst aichat--http-response-status-line-regexp
  (rx "HTTP/" (group (or "1.0" "1.1" "2")) " "
      ;; Status code
      (group (1+ digit)) " "
      ;; Reason phrase
      (optional (group (1+ (not (any "\r\n")))))
      (or
       ;; HTTP 1
       "\r\n"
       ;; HTTP 2
       "\n"))
  "Regular expression matching HTTP response status line.")

(defconst aichat--http-end-of-headers-regexp
  (rx (or "\r\n\r\n" "\n\n" "\r\n\n"))
  "Regular expression matching the end of HTTP headers.
This must work with both HTTP/1 (using CRLF) and HTTP/2 (using
only LF).")

(defun aichat--http-urlencode-alist (alist)
  "Hexify ALIST fields according to RFC3986."
  (cl-loop for sep = "" then "&"
           for (k . v) in alist
           concat sep
           concat (url-hexify-string (format "%s" k))
           concat "="
           concat (url-hexify-string (format "%s" v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; url-backend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aichat--http-report-data (&rest _)
  "Report http data for stream request."
  ;; (message "buffer:\n|%s|" (buffer-string))
  ;; (message "report point - point-max:\n|%s|" (buffer-substring aichat--http-report-point (point-max)))
  (unless aichat--http-reported-header-p
    (save-mark-and-excursion
      (goto-char (point-min))
      (when (re-search-forward "^HTTP/[1-9]\\.?[0-9]? \\([0-9]\\{3\\}\\) \\([a-zA-Z ]*\\)" url-http-end-of-headers t)
        (setq-local aichat--http-response-status (cons (match-string 1) (match-string 2)))
        (when aichat--http-response-callback
          (funcall aichat--http-response-callback 'status aichat--http-response-status)))
      (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                                url-http-end-of-headers t)
        (let ((header-key (match-string 1))
              (header-value (match-string 2)))
          (push (cons header-key header-value)
                aichat--http-response-headers)
          (when (string= "content-length" (downcase header-key))
            (setq-local aichat--http-response-chunked-p nil))))
      (when aichat--http-response-callback
        (funcall aichat--http-response-callback 'headers aichat--http-response-headers)))
    (setq-local aichat--http-report-point (min (+ 1 url-http-end-of-headers) (point-max)))
    (setq-local aichat--http-reported-header-p t))
  (when-let* ((point-end (if aichat--http-response-chunked-p (- (point-max) 2) (point-max)))
              (body (buffer-substring aichat--http-report-point point-end)))
    (unless (string-empty-p body)
      (when aichat--http-response-callback
        (funcall aichat--http-response-callback 'body body))
      (setq-local aichat--http-response-body (concat aichat--http-response-body body)))
    (setq-local aichat--http-report-point point-end)))

;;; The following functions are copied from the url-http.el

(defun aichat--url-http-create-request ()
  "Create an HTTP request for `url-http-target-url'.
Use `url-http-referer' as the Referer-header (subject to `url-privacy-level')."
  (let* ((extra-headers)
	     (request nil)
	     (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
	     (using-proxy url-http-proxy)
	     (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
					                          url-http-extra-headers))
			                 (not using-proxy))
			             nil
		               (let ((url-basic-auth-storage
			                  'url-http-proxy-basic-auth-storage))
			             (url-get-authentication url-http-proxy nil 'any nil))))
	     (real-fname (url-filename url-http-target-url))
	     (host (url-host url-http-target-url))
	     (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
		           nil
		         (url-get-authentication (or
					                      (and (boundp 'proxy-info)
					                           proxy-info)
					                      url-http-target-url) nil 'any nil)))
         (ref-url (url-http--encode-string url-http-referer)))
    (if (equal "" real-fname)
	    (setq real-fname "/"))
    (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
    (if auth
	    (setq auth (concat "Authorization: " auth "\r\n")))
    (if proxy-auth
	    (setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))

    ;; Protection against stupid values in the referrer
    (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
					                       (string= ref-url "")))
	    (setq ref-url nil))

    ;; url-http-extra-headers contains an assoc-list of
    ;; header/value pairs that we need to put into the request.
    (setq extra-headers (mapconcat
			             (lambda (x)
			               (concat (car x) ": " (cdr x)))
			             url-http-extra-headers "\r\n"))
    (if (not (equal extra-headers ""))
	    (setq extra-headers (concat extra-headers "\r\n")))

    ;; This was done with a call to `format'.  Concatenating parts has
    ;; the advantage of keeping the parts of each header together and
    ;; allows us to elide null lines directly, at the cost of making
    ;; the layout less clear.
    (setq request
          (concat
           ;; The request
           (or url-http-method "GET") " "
           (url-http--encode-string
            (if (and using-proxy
                     ;; Bug#35969.
                     (not (equal "https" (url-type url-http-target-url))))
                (let ((url (copy-sequence url-http-target-url)))
                  (setf (url-host url) (puny-encode-domain (url-host url)))
                  (url-recreate-url url))
              real-fname))
           " HTTP/" url-http-version "\r\n"
           ;; Version of MIME we speak
           "MIME-Version: 1.0\r\n"
           ;; (maybe) Try to keep the connection open
           "Connection: " (if (or using-proxy
                                  (not url-http-attempt-keepalives))
                              "close" "keep-alive") "\r\n"
           ;; HTTP extensions we support
           (if url-extensions-header
               (format
                "Extension: %s\r\n" url-extensions-header))
           ;; Who we want to talk to
           (if (/= (url-port url-http-target-url)
                   (url-scheme-get-property
                    (url-type url-http-target-url) 'default-port))
               (format
                "Host: %s:%d\r\n" (url-http--encode-string
                                   (puny-encode-domain host))
                (url-port url-http-target-url))
             (format "Host: %s\r\n"
                     (url-http--encode-string (puny-encode-domain host))))
           ;; Who its from
           (if url-personal-mail-address
               (concat
                "From: " url-personal-mail-address "\r\n"))
           ;; Encodings we understand
           (if (or url-mime-encoding-string
		           ;; MS-Windows loads zlib dynamically, so recheck
		           ;; in case they made it available since
		           ;; initialization in url-vars.el.
		           (and (eq 'system-type 'windows-nt)
			            (fboundp 'zlib-available-p)
			            (zlib-available-p)
			            (setq url-mime-encoding-string "gzip")))
               (concat
                "Accept-encoding: " url-mime-encoding-string "\r\n"))
           (if url-mime-charset-string
               (concat
                "Accept-charset: "
                (url-http--encode-string url-mime-charset-string)
                "\r\n"))
           ;; Languages we understand
           (if url-mime-language-string
               (concat
                "Accept-language: " url-mime-language-string "\r\n"))
           ;; Types we understand
           "Accept: " (or url-mime-accept-string "*/*") "\r\n"
           ;; User agent
           (url-http-user-agent-string)
           ;; Proxy Authorization
           proxy-auth
           ;; Authorization
           auth
           ;; Cookies
	       (when (url-use-cookies url-http-target-url)
             (url-http--encode-string
              (url-cookie-generate-header-lines
               host real-fname
               (equal "https" (url-type url-http-target-url)))))
           ;; If-modified-since
           (if (and (not no-cache)
                    (member url-http-method '("GET" nil)))
               (let ((tm (url-is-cached url-http-target-url)))
                 (if tm
                     (concat "If-modified-since: "
                             (url-get-normalized-date tm) "\r\n"))))
           ;; Whence we came
           (if ref-url (concat
                        "Referer: " ref-url "\r\n"))
           extra-headers
           ;; Length of data
           (if url-http-data
               (concat
                "Content-length: " (number-to-string
                                    (length (encode-coding-string url-http-data 'utf-8)))
                "\r\n"))
           ;; End request
           "\r\n"
           ;; Any data
           url-http-data))
    ;; Bug#23750
    ;; (unless (= (string-bytes request)
    ;;            (length request))
    ;; (error "Multibyte text in HTTP request: %s" request))
    (url-http-debug "Request is: \n%s" request)
    request))

(defun aichat--url-display-message (fmt &rest args)
  "Like `message', but do nothing if `url-show-status' is nil."
  (when (and url-show-status
             (not (and url-current-object (url-silent url-current-object)))
             fmt)
    (apply #'message fmt args)))

(defun aichat--url-http-content-length-after-change-function (_st nd _length)
  "Function used when we DO know how long the document is going to be.
More sophisticated percentage downloaded, etc.
Also does minimal parsing of HTTP headers and will actually cause
the callback to be triggered."
  (if url-http-content-type
      (aichat--url-display-message
       "Reading [%s]... %s of %s (%d%%)"
       url-http-content-type
       (funcall byte-count-to-string-function (- nd url-http-end-of-headers))
       (funcall byte-count-to-string-function url-http-content-length)
       (url-percentage (- nd url-http-end-of-headers)
		               url-http-content-length))
    (aichat--url-display-message
     "Reading... %s of %s (%d%%)"
     (funcall byte-count-to-string-function (- nd url-http-end-of-headers))
     (funcall byte-count-to-string-function url-http-content-length)
     (url-percentage (- nd url-http-end-of-headers)
		             url-http-content-length)))

  (if (> (- nd url-http-end-of-headers) url-http-content-length)
      (progn
	    ;; Found the end of the document!  Wheee!
	    (url-lazy-message "Reading... done.")
	    (if (url-http-parse-headers)
            ;; add this line
            (progn
              (aichat--http-report-data)
	          (url-http-activate-callback))))))

(defun aichat--url-http-chunked-encoding-after-change-function (st nd length)
  "Function used when dealing with chunked encoding.
Cannot give a sophisticated percentage, but we need a different
function to look for the special 0-length chunk that signifies
the end of the document."
  (if url-http-chunked-last-crlf-missing
      (progn
        (goto-char url-http-chunked-last-crlf-missing)
        (if (not (looking-at "\r\n"))
	        (url-http-debug
             "Still spinning for the terminator of last chunk...")
          (url-http-debug "Saw the last CRLF.")
          (delete-region (match-beginning 0) (match-end 0))
          (when (url-http-parse-headers)
	        (url-http-activate-callback))))
    (save-excursion
      (goto-char st)
      (let ((read-next-chunk t)
	        (case-fold-search t)
	        (regexp nil)
	        (no-initial-crlf nil))
        ;; We need to loop thru looking for more chunks even within
        ;; one after-change-function call.
        (while read-next-chunk
	      (setq no-initial-crlf (= 0 url-http-chunked-counter))
	      (url-http-debug "Reading chunk %d (%d %d %d)"
			              url-http-chunked-counter st nd length)
	      (setq regexp (if no-initial-crlf
			               "\\([0-9a-z]+\\).*\r?\n"
		                 "\r?\n\\([0-9a-z]+\\).*\r?\n"))

	      (if url-http-chunked-start
	          ;; We know how long the chunk is supposed to be, skip over
	          ;; leading crap if possible.
	          (if (> nd (+ url-http-chunked-start url-http-chunked-length))
		          (progn
		            (url-http-debug "Got to the end of chunk #%d!"
				                    url-http-chunked-counter)
		            (goto-char (+ url-http-chunked-start
				                  url-http-chunked-length)))
	            (url-http-debug "Still need %d bytes to hit end of chunk"
			                    (- (+ url-http-chunked-start
				                      url-http-chunked-length)
				                   nd))
	            (setq read-next-chunk nil)))
	      (if (not read-next-chunk)
	          (url-http-debug "Still spinning for next chunk...")
	        (if no-initial-crlf (skip-chars-forward "\r\n"))
	        (if (not (looking-at regexp))
	            (progn
	              ;; Must not have received the entirety of the chunk header,
		          ;; need to spin some more.
		          (url-http-debug "Did not see start of chunk @ %d!" (point))
		          (setq read-next-chunk nil))
              ;; The data we got may have started in the middle of the
              ;; initial chunk header, so move back to the start of the
              ;; line and re-compute.
              (when (= url-http-chunked-counter 0)
                (beginning-of-line)
                (looking-at regexp))
              (add-text-properties (match-beginning 0) (match-end 0)
                                   (list 'chunked-encoding t
				                         'face 'cursor
				                         'invisible t))
	          (setq url-http-chunked-length
                    (string-to-number (buffer-substring (match-beginning 1)
                                                        (match-end 1))
                                      16)
		            url-http-chunked-counter (1+ url-http-chunked-counter)
		            url-http-chunked-start (set-marker
					                        (or url-http-chunked-start
					                            (make-marker))
					                        (match-end 0)))
	          (delete-region (match-beginning 0) (match-end 0))
	          (url-http-debug "Saw start of chunk %d (length=%d, start=%d"
			                  url-http-chunked-counter url-http-chunked-length
			                  (marker-position url-http-chunked-start))

              ;; add this line
              (aichat--http-report-data)

	          (if (= 0 url-http-chunked-length)
		          (progn
		            ;; Found the end of the document!  Wheee!
		            (url-http-debug "Saw end of stream chunk!")
		            (setq read-next-chunk nil)
		            ;; Every chunk, even the last 0-length one, is
		            ;; terminated by CRLF.  Skip it.
		            (if (not (looking-at "\r?\n"))
                        (progn
	                      (url-http-debug
                           "Spinning for the terminator of last chunk...")
                          (setq url-http-chunked-last-crlf-missing
                                (point)))
		              (url-http-debug "Removing terminator of last chunk")
		              (delete-region (match-beginning 0) (match-end 0))
		              (when (re-search-forward "^\r?\n" nil t)
		                (url-http-debug "Saw end of trailers..."))
		              (when (url-http-parse-headers)
		                (url-http-activate-callback))))))))))))

(defun aichat--url-http-wait-for-headers-change-function (_st nd _length)
  ;; This will wait for the headers to arrive and then splice in the
  ;; next appropriate after-change-function, etc.
  (url-http-debug "url-http-wait-for-headers-change-function (%s)"
		          (buffer-name))
  (let ((end-of-headers nil)
	    (old-http nil)
	    (process-buffer (current-buffer))
	    ;; (content-length nil)
        )
    (when (not (bobp))
      (goto-char (point-min))
      (if (and (looking-at ".*\n")	; have one line at least
	           (not (looking-at "^HTTP/[1-9]\\.[0-9]")))
	      ;; Not HTTP/x.y data, must be 0.9
	      ;; God, I wish this could die.
	      (setq end-of-headers t
		        url-http-end-of-headers 0
		        old-http t)
	    ;; Blank line at end of headers.
	    (when (re-search-forward "^\r?\n" nil t)
	      (backward-char 1)
	      ;; Saw the end of the headers
	      (url-http-debug "Saw end of headers... (%s)" (buffer-name))
	      (setq url-http-end-of-headers (set-marker (make-marker)
						                            (point))
		        end-of-headers t)
	      (setq nd (- nd (url-http-clean-headers)))))

      (if (not end-of-headers)
	      ;; Haven't seen the end of the headers yet, need to wait
	      ;; for more data to arrive.
	      nil
	    (unless old-http
	      (url-http-parse-response)
	      (mail-narrow-to-head)
	      (setq url-http-transfer-encoding (mail-fetch-field
					                        "transfer-encoding")
		        url-http-content-type (mail-fetch-field "content-type"))
	      (if (mail-fetch-field "content-length")
	          (setq url-http-content-length
		            (string-to-number (mail-fetch-field "content-length"))))
	      (widen))
	    (when url-http-transfer-encoding
	      (setq url-http-transfer-encoding
		        (downcase url-http-transfer-encoding)))

	    (cond
	     ((null url-http-response-status)
	      ;; We got back a headerless malformed response from the
	      ;; server.
	      (url-http-activate-callback))
	     ((memq url-http-response-status '(204 205))
	      (url-http-debug "%d response must have headers only (%s)."
			              url-http-response-status (buffer-name))
	      (when (url-http-parse-headers)
	        (url-http-activate-callback)))
	     ((string= "HEAD" url-http-method)
	      ;; A HEAD request is _ALWAYS_ terminated by the header
	      ;; information, regardless of any entity headers,
	      ;; according to section 4.4 of the HTTP/1.1 draft.
	      (url-http-debug "HEAD request must have headers only (%s)."
			              (buffer-name))
	      (when (url-http-parse-headers)
	        (url-http-activate-callback)))
	     ((string= "CONNECT" url-http-method)
	      ;; A CONNECT request is finished, but we cannot stick this
	      ;; back on the free connection list
	      (url-http-debug "CONNECT request must have headers only.")
	      (when (url-http-parse-headers)
	        (url-http-activate-callback)))
	     ((equal url-http-response-status 304)
	      ;; Only allowed to have a header section.  We have to handle
	      ;; this here instead of in url-http-parse-headers because if
	      ;; you have a cached copy of something without a known
	      ;; content-length, and try to retrieve it from the cache, we'd
	      ;; fall into the 'being dumb' section and wait for the
	      ;; connection to terminate, which means we'd wait for 10
	      ;; seconds for the keep-alives to time out on some servers.
	      (when (url-http-parse-headers)
	        (url-http-activate-callback)))
	     (old-http
	      ;; HTTP/0.9 always signaled end-of-connection by closing the
	      ;; connection.
	      (url-http-debug
	       "Saw HTTP/0.9 response, connection closed means end of document.")
	      (setq url-http-after-change-function
		        #'url-http-simple-after-change-function))
	     ((equal url-http-transfer-encoding "chunked")
	      (url-http-debug "Saw chunked encoding.")
	      (setq url-http-after-change-function
		        #'aichat--url-http-chunked-encoding-after-change-function)
	      (when (> nd url-http-end-of-headers)
	        (url-http-debug
	         "Calling initial chunked-encoding for extra data at end of headers")
	        (aichat--url-http-chunked-encoding-after-change-function
	         (marker-position url-http-end-of-headers) nd
	         (- nd url-http-end-of-headers))))
	     ((integerp url-http-content-length)
	      (url-http-debug
	       "Got a content-length, being smart about document end.")
	      (setq url-http-after-change-function
		        #'aichat--url-http-content-length-after-change-function)
	      (cond
	       ((= 0 url-http-content-length)
	        ;; We got a NULL body!  Activate the callback
	        ;; immediately!
	        (url-http-debug
	         "Got 0-length content-length, activating callback immediately.")
	        (when (url-http-parse-headers)
	          (url-http-activate-callback)))
	       ((> nd url-http-end-of-headers)
	        ;; Have some leftover data
	        (url-http-debug "Calling initial content-length for extra data at end of headers")
	        (aichat--url-http-content-length-after-change-function
	         (marker-position url-http-end-of-headers)
	         nd
	         (- nd url-http-end-of-headers)))
	       (t
	        nil)))
	     (t
	      (url-http-debug "No content-length, being dumb.")
	      (setq url-http-after-change-function
		        #'url-http-simple-after-change-function)))))
    ;; We are still at the beginning of the buffer... must just be
    ;; waiting for a response.
    (url-http-debug "Spinning waiting for headers...")
    (when (eq process-buffer (current-buffer))
      (goto-char (point-max)))))

(defun aichat--url-https-proxy-after-change-function (_st _nd _length)
  (let* ((process-buffer (current-buffer))
         (proc (get-buffer-process process-buffer)))
    (goto-char (point-min))
    (when (re-search-forward "^\r?\n" nil t)
      (backward-char 1)
      ;; Saw the end of the headers
      (setq url-http-end-of-headers (set-marker (make-marker) (point)))
      (url-http-parse-response)
      (cond
       ((null url-http-response-status)
        ;; We got back a headerless malformed response from the
        ;; server.
        (url-http-activate-callback)
        (error "Malformed response from proxy, fail!"))
       ((= url-http-response-status 200)
        (if (gnutls-available-p)
            (condition-case e
                (let ((tls-connection (gnutls-negotiate
                                       :process proc
                                       :hostname (puny-encode-domain (url-host url-current-object))
                                       :verify-error nil)))
                  ;; check certificate validity
                  (setq tls-connection
                        (nsm-verify-connection tls-connection
                                               (puny-encode-domain (url-host url-current-object))
                                               (url-port url-current-object)))
                  (with-current-buffer process-buffer (erase-buffer))
                  (set-process-buffer tls-connection process-buffer)
                  (setq url-http-after-change-function
                        #'aichat--url-http-wait-for-headers-change-function)
                  (set-process-filter tls-connection 'url-http-generic-filter)
                  (process-send-string tls-connection
                                       (aichat--url-http-create-request)))
              (gnutls-error
               (url-http-activate-callback)
               (error "gnutls-error: %s" e))
              (error
               (url-http-activate-callback)
               (error "Error: %s" e)))
          (error "Error: gnutls support needed!")))
       (t
        (url-http-debug "error response: %d" url-http-response-status)
        (url-http-activate-callback))))))

(defun aichat--url-https-proxy-connect (connection)
  (setq url-http-after-change-function #'aichat--url-https-proxy-after-change-function)
  (process-send-string
   connection
   (format
    (concat "CONNECT %s:%d HTTP/1.1\r\n"
            "Host: %s\r\n"
            (let ((proxy-auth (let ((url-basic-auth-storage
                                     'url-http-proxy-basic-auth-storage))
                                (url-get-authentication url-http-proxy nil
                                                        'any nil))))
              (and proxy-auth
                   (concat "Proxy-Authorization: " proxy-auth "\r\n")))
            "\r\n")
    (puny-encode-domain (url-host url-current-object))
    (or (url-port url-current-object)
        url-https-default-port)
    (puny-encode-domain (url-host url-current-object)))))

(defun aichat--url-http-async-sentinel (proc why)
  ;; We are performing an asynchronous connection, and a status change
  ;; has occurred.
  (when (buffer-name (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (cond
       (url-http-connection-opened
	    (setq url-http-no-retry t)
	    (url-http-end-of-document-sentinel proc why))
       ((string= (substring why 0 4) "open")
	    (setq url-http-connection-opened t)
        (if (and url-http-proxy (string= "https" (url-type url-current-object)))
            (aichat--url-https-proxy-connect proc)
          (condition-case error
              (process-send-string proc (aichat--url-http-create-request))
            (file-error
             (setq url-http-connection-opened nil)
             (message "HTTP error: %s" error)))))
       (t
	    (setf (car url-callback-arguments)
	          (nconc (list :error (list 'error 'connection-failed why
					                    :host (url-host (or url-http-proxy url-current-object))
					                    :service (url-port (or url-http-proxy url-current-object))))
		             (car url-callback-arguments)))
	    (url-http-activate-callback))))))

(defun aichat--url-interactive-p ()
  "Non-nil when the current request is from an interactive context."
  (not (or url-request-noninteractive
           (bound-and-true-p url-http-noninteractive))))

(defconst url-aichat--http-default-port url-http-default-port)
(defconst url-aichat--http-asynchronous-p  url-http-asynchronous-p)
(defalias 'url-aichat--http-expand-file-name #'url-http-expand-file-name)
(defalias 'url-aichat--http-file-exists-p #'url-http-file-exists-p)
(defalias 'url-aichat--http-file-readable-p #'url-http-file-readable-p)
(defalias 'url-aichat--http-file-attributes #'url-http-file-attributes)

(defun url-aichat--http (url callback cbargs &optional retry-buffer gateway-method)
  "Retrieve URL via HTTP asynchronously.
URL must be a parsed URL.  See `url-generic-parse-url' for details.

When retrieval is completed, execute the function CALLBACK,
passing it an updated value of CBARGS as arguments.  The first
element in CBARGS should be a plist describing what has happened
so far during the request, as described in the docstring of
`url-retrieve' (if in doubt, specify nil).  The current buffer
when CALLBACK is executed is the retrieval buffer.

Optional arg RETRY-BUFFER, if non-nil, specifies the buffer of a
previous `url-http' call, which is being re-attempted.

Optional arg GATEWAY-METHOD specifies the gateway to be used,
overriding the value of `url-gateway-method'.

The return value of this function is the retrieval buffer."
  (cl-check-type url url "Need a pre-parsed URL.")
  (setf (url-type url) (cadr (split-string (url-type url) "--")))
  (let* (;; (host (url-host (or url-using-proxy url)))
	     ;; (port (url-port (or url-using-proxy url)))
	     (nsm-noninteractive (not (aichat--url-interactive-p)))
         ;; The following binding is needed in url-open-stream, which
         ;; is called from url-http-find-free-connection.
         (url-current-object url)
         (connection (url-http-find-free-connection (url-host url)
                                                    (url-port url)
                                                    gateway-method))
         (mime-accept-string url-mime-accept-string)
	     (buffer (or retry-buffer
		             (generate-new-buffer
                      (format " *http %s:%d*" (url-host url) (url-port url)))))
         (referer (url-http--encode-string (url-http--get-referer url))))
    (if (not connection)
	    ;; Failed to open the connection for some reason
	    (progn
	      (kill-buffer buffer)
	      (setq buffer nil)
          (error "Could not create connection to %s:%d" (url-host url)
                 (url-port url)))
      (with-current-buffer buffer
	    (mm-disable-multibyte)
	    (setq url-current-object url
	          mode-line-format "%b [%s]")

	    (dolist (var '(url-http-end-of-headers
		               url-http-content-type
		               url-http-content-length
		               url-http-transfer-encoding
		               url-http-after-change-function
		               url-http-response-version
		               url-http-response-status
                       url-http-chunked-last-crlf-missing
		               url-http-chunked-length
		               url-http-chunked-counter
		               url-http-chunked-start
		               url-callback-function
		               url-callback-arguments
		               url-show-status
		               url-http-process
		               url-http-method
		               url-http-extra-headers
		               url-http-noninteractive
		               url-http-data
		               url-http-target-url
		               url-http-no-retry
		               url-http-connection-opened
                       url-mime-accept-string
		               url-http-proxy
                       url-http-referer))
	      (set (make-local-variable var) nil))

	    (setq url-http-method (or url-request-method "GET")
	          url-http-extra-headers url-request-extra-headers
	          url-http-noninteractive url-request-noninteractive
	          url-http-data url-request-data
	          url-http-process connection
              url-http-chunked-last-crlf-missing nil
	          url-http-chunked-length nil
	          url-http-chunked-start nil
	          url-http-chunked-counter 0
	          url-callback-function callback
	          url-callback-arguments cbargs
	          url-http-after-change-function 'aichat--url-http-wait-for-headers-change-function
	          url-http-target-url url-current-object
	          url-http-no-retry retry-buffer
	          url-http-connection-opened nil
              url-mime-accept-string mime-accept-string
	          url-http-proxy url-using-proxy
              url-http-referer referer)

	    (set-process-buffer connection buffer)
	    (set-process-filter connection #'url-http-generic-filter)
	    (pcase (process-status connection)
          ('connect
           ;; Asynchronous connection
           (set-process-sentinel connection 'aichat--url-http-async-sentinel))
          ('failed
           ;; Asynchronous connection failed
           (error "Could not create connection to %s:%d" (url-host url)
                  (url-port url)))
          (_
           (if (and url-http-proxy (string= "https"
                                            (url-type url-current-object)))
               (aichat--url-https-proxy-connect connection)
             (set-process-sentinel connection
                                   #'url-http-end-of-document-sentinel)
             (process-send-string connection (aichat--url-http-create-request)))))))
    buffer))


(defmacro aichat--url-https-create-secure-wrapper (method args)
  `(defun ,(intern (format (if method "url-aichat--https-%s" "url-aichat--https") method)) ,args
     ,(format "HTTPS wrapper around `%s' call." (or method "url-aichat--http"))
     (,(intern (format (if method "url-aichat--http-%s" "url-aichat--http") method))
      ,@(remove '&rest (remove '&optional (append args (if method nil '(nil 'tls))))))))

(defconst url-aichat--https-default-port url-https-default-port)
(defconst url-aichat--https-asynchronous-p  url-https-asynchronous-p)
(defalias 'url-aichat--https-expand-file-name #'url-https-expand-file-name)
(aichat--url-https-create-secure-wrapper nil (url callback cbargs))
(aichat--url-https-create-secure-wrapper file-exists-p (url))
(aichat--url-https-create-secure-wrapper file-readable-p (url))
(aichat--url-https-create-secure-wrapper file-attributes (url &optional id-format))


(defun aichat--url-proxy (url callback &optional cbargs)
  ;; Retrieve URL from a proxy.
  ;; Expects `url-using-proxy' to be bound to the specific proxy to use."
  (setq url-using-proxy (url-generic-parse-url url-using-proxy))

  (cond
   ((string= (url-type url-using-proxy) "http")
    (url-aichat--http url callback cbargs))
   (t
    (error "Don't know how to use proxy `%s'" url-using-proxy))))

(defun aichat--url-retrieve-internal (url callback cbargs &optional silent
				                          inhibit-cookies)
  "Internal function; external interface is `url-retrieve'.
The callback function will receive an updated value of CBARGS as
arguments; its first element should be a plist specifying what has
happened so far during the request, as described in the docstring
of `url-retrieve' (if in doubt, specify nil).

If SILENT, don't message progress reports and the like.
If INHIBIT-COOKIES, cookies will neither be stored nor sent to
the server.
If URL is a multibyte string, it will be encoded as utf-8 and
URL-encoded before it's used."
  (url-do-setup)
  (url-gc-dead-buffers)
  (when (stringp url)
    (set-text-properties 0 (length url) nil url)
    (setq url (url-encode-url url)))
  (if (not (url-p url))
      (setq url (url-generic-parse-url url)))
  (if (not (functionp callback))
      (error "Must provide a callback function to url-retrieve"))
  (unless (url-type url)
    (error "Bad url: %s" (url-recreate-url url)))
  (setf (url-silent url) silent)
  (setf (url-asynchronous url) url-asynchronous)
  (setf (url-use-cookies url) (not inhibit-cookies))
  ;; Once in a while, remove old entries from the URL cache.
  (when (zerop (% url-retrieve-number-of-calls 1000))
    (condition-case error
	    (url-cache-prune-cache)
      (file-error
       (message "Error when expiring the cache: %s" error))))
  (setq url-retrieve-number-of-calls (1+ url-retrieve-number-of-calls))
  (let ((loader (url-scheme-get-property (url-type url) 'loader))
	    (url-using-proxy (if (url-host url)
			                 (url-find-proxy-for-url url (url-host url))))
	    (buffer nil)
	    (asynch (url-scheme-get-property (url-type url) 'asynchronous-p)))
    (when url-using-proxy
      (setf asynch t
	        loader #'aichat--url-proxy
            (url-asynchronous url) t))
    (if asynch
	    (let ((url-current-object url))
	      (setq buffer (funcall loader url callback cbargs)))
      (setq buffer (funcall loader url))
      (if buffer
	      (with-current-buffer buffer
	        (apply callback cbargs))))
    (if url-history-track
	    (url-history-update-url url (current-time)))
    buffer))

(defun aichat--url-retrieve (url callback &optional cbargs silent inhibit-cookies)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished.
URL is either a string or a parsed URL.  If it is a string
containing characters that are not valid in a URI, those
characters are percent-encoded; see `url-encode-url'.

CALLBACK is called when the object has been completely retrieved, with
the current buffer containing the object, and any MIME headers associated
with it.  It is called as (apply CALLBACK STATUS CBARGS).
STATUS is a plist representing what happened during the request,
with most recent events first, or an empty list if no events have
occurred.  Each pair is one of:

\(:redirect REDIRECTED-TO) - the request was redirected to this URL.

\(:error (error type . DATA)) - an error occurred.  TYPE is a
symbol that says something about where the error occurred, and
DATA is a list (possibly nil) that describes the error further.

Return the buffer URL will load into, or nil if the process has
already completed (i.e. URL was a mailto URL or similar; in this case
the callback is not called).

The variables `url-request-data', `url-request-method' and
`url-request-extra-headers' can be dynamically bound around the
request; dynamic binding of other variables doesn't necessarily
take effect.

If SILENT, then don't message progress reports and the like.
If INHIBIT-COOKIES, cookies will neither be stored nor sent to
the server.
If URL is a multibyte string, it will be encoded as utf-8 and
URL-encoded before it's used."
  ;; XXX: There is code in Emacs that does dynamic binding
  ;; of the following variables around url-retrieve:
  ;; url-standalone-mode, url-gateway-unplugged,
  ;; url-confirmation-func, url-cookie-multiple-line,
  ;; url-cookie-{{,secure-}storage,confirmation}
  ;; url-standalone-mode and url-gateway-unplugged should work as
  ;; usual.  url-confirmation-func is only used in nnwarchive.el and
  ;; webmail.el; the latter should be updated.  Is
  ;; url-cookie-multiple-line needed anymore?  The other url-cookie-*
  ;; are (for now) only used in synchronous retrievals.
  (aichat--url-retrieve-internal url callback (cons nil cbargs) silent
			                     inhibit-cookies))

(cl-defun aichat--url-http (url &rest settings
                                &key
                                (proxy nil)
                                (type nil)
                                (params nil)
                                (headers nil)
                                (data nil)
                                (callback-data nil)
                                (callback nil)
                                &allow-other-keys)
  "Request URL with property list SETTINGS as follow. Return value is promise,
the format of resolve value is (resp-status resp-headers resp-body).

===================== ======================================================
Keyword argument      Explanation
===================== ======================================================
PROXY         (string)   proxy of current request.
TYPE          (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS         (alist)   set \"?key=val\" part in URL
HEADERS        (alist)   additional headers to send with the request
DATA          (string)   data to be sent to the server
CALLBACK-DATA (object)   data to be used on CALLBACK by aichat--http-callack-data
CALLBACK      (string)   callbacl to receive reported http data.
"
  (when params
    (cl-assert (listp params) nil "PARAMS must be an alist.  Given: %S" params)
    (setq url (concat url (if (string-match-p "\\?" url) "&" "?")
                      (aichat--http-urlencode-alist params))))
  (setq url (concat "aichat--" url))
  (unless type
    (setq type "GET"))
  (promise-new (lambda (resolve reject)
                 (condition-case error
                     (let ((url-proxy-services (if proxy
                                                   (cons (cons  (car (split-string url ":")) proxy) url-proxy-services)
                                                 url-proxy-services))
                           (url-user-agent aichat-user-agent)
                           (url-request-extra-headers headers)
                           (url-request-method type)
                           (url-request-data data))
                       (with-current-buffer
                           (aichat--url-retrieve url
                                                 (lambda (status)
                                                   ;;(display-buffer (current-buffer))
                                                   (let ((err (plist-get status :error)))
                                                     (if err
                                                         (funcall reject err)
                                                       (funcall resolve (list aichat--http-response-status
                                                                              aichat--http-response-headers
                                                                              aichat--http-response-body)))))
                                                 nil t)
                         (set (make-local-variable 'url-user-agent) aichat-user-agent)
                         (setq-local aichat--http-response-callback callback)
                         (setq-local aichat--http-callback-data callback-data)
                         (setq-local aichat--http-response-status nil)
                         (setq-local aichat--http-response-headers nil)
                         (setq-local aichat--http-response-body nil)
                         (setq-local aichat--http-reported-header-p nil)
                         (setq-local aichat--http-report-point 1)
                         (setq-local aichat--http-response-chunked-p t)))
                   (error (funcall reject error))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; curl-backend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom aichat-curl-program "curl"
  "The path of curl program."
  :group 'aichat
  :type 'string)

(defun aichat--curl-process-filter (proc data)
  (aichat-debug "curl filter data: \n%s" data)
  (let ((buffer (process-buffer proc)))
    (when (and (buffer-live-p buffer) (not (zerop (length data))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert data)
        (goto-char aichat--curl-parser-point)
        (while (not (= (point) (point-max)))
          (when (eq 'status aichat--curl-parser-state)
            (if (not (re-search-forward aichat--http-response-status-line-regexp nil t))
                (goto-char (point-max)) ;; wait for more data
              (setq-local aichat--curl-response-status (cons (match-string 2) (match-string 3)))
              (setq-local aichat--curl-parser-point (point))

              (unless (string= "Connection established" (cdr aichat--curl-response-status))
                ;; proxy, wait for next status line
                (when aichat--curl-response-callback
                  (funcall aichat--curl-response-callback 'status aichat--curl-response-status))
                (setq-local aichat--curl-parser-state 'header))))

          (when (eq 'header aichat--curl-parser-state)
            (if  (not (re-search-forward aichat--http-end-of-headers-regexp nil t))
                (goto-char (point-max)) ;; wait for more data
              (let* ((bound (point))
                     (lines (split-string (buffer-substring aichat--curl-parser-point bound) "[\f\t\n\r\v]+")))
                (mapc (lambda (line)
                        (let ((kv (split-string line ": ")))
                          (push (cons (car kv) (cadr kv))
                                aichat--curl-response-headers)))
                      lines)
                (setq-local aichat--curl-parser-point bound)
                (when aichat--curl-response-callback
                  (funcall aichat--curl-response-callback 'headers aichat--curl-response-headers))
                (setq-local aichat--curl-parser-state 'body))))

          (when (eq 'body aichat--curl-parser-state)
            (let ((body (buffer-substring aichat--curl-parser-point (point-max))))
              (when aichat--curl-response-callback
                (funcall aichat--curl-response-callback 'body body))
              (setq-local aichat--curl-response-body (concat aichat--curl-response-body body))
              (setq-local aichat--curl-parser-point (point-max))
              ;; wait for more data
              (goto-char (point-max)))))))))

(defun aichat--curl-process-sentinel (proc status)
  (aichat-debug "curl process sentinel: %s" status)
  (with-current-buffer (process-buffer proc)
    (unwind-protect
        (let ((stderr-str (with-current-buffer aichat--curl-stderr
                            (buffer-string))))
          (if (and (string= status "finished\n") (string-empty-p stderr-str))
              (funcall aichat--curl-resolve (list aichat--curl-response-status aichat--curl-response-headers aichat--curl-response-body))
            (funcall aichat--curl-reject (list status stderr-str))))
      (funcall aichat--curl-cleanup))))


(defun aichat--curl-make-config (url type headers proxy data)
  (concat
   (format "url = \"%s\"\n" url)
   (format "request = \"%s\"\n" (if type type "GET"))
   (format "user-agent = \"%s\"\n" aichat-user-agent)
   (when-let* ((url-object (url-generic-parse-url url))
               (host (url-host url-object))
               (url-type (url-type url-object))
               (filename (url-filename url-object))
               (cookies (url-cookie-generate-header-lines host (if (string-empty-p filename) "/" filename) (equal "https" url-type))))
     (unless (string-empty-p cookies)
       (format "header = \"%s\"\n" (string-trim-right cookies))))

   (when headers
     (cl-loop for (key . value) in headers
              concat (format "header = %s\n" (aichat-json-serialize (concat key ": " value)))))
   (when proxy
     (format "proxy = http://%s\n" proxy))
   (when data
     (format "data = %s\n" (aichat-json-serialize data)))))

(cl-defun aichat--curl-http (url &rest settings
                                 &key
                                 (proxy nil)
                                 (type nil)
                                 (params nil)
                                 (headers nil)
                                 (data nil)
                                 (callback-data nil)
                                 (callback nil)
                                 &allow-other-keys)
  (when params
    (setq url (concat url (if (string-match-p "\\?" url) "&" "?")
                      (aichat--http-urlencode-alist params))))
  (promise-new (lambda (resolve reject)
                 (let* ((command (append (list aichat-curl-program "--silent" "--show-error" "--include" "--no-styled-output")
                                         (when callback
                                           (list "--no-buffer"))
                                         (list "--config" "-")))
                        (config (aichat--curl-make-config url type headers proxy data))
                        (program (car command))
                        (stdout (generate-new-buffer (concat "*" program "-stdout*")))
                        (stderr (generate-new-buffer (concat "*" program "-stderr*")))
                        (stderr-pipe-name (concat "*" program "-stderr-pipe*"))
                        (stderr-pipe (make-pipe-process
                                      :name stderr-pipe-name
                                      :noquery t
                                      :filter (lambda (_ output)
                                                (with-current-buffer stderr
                                                  (insert output)))))
                        (cleanup (lambda ()
                                   (when (buffer-live-p stdout)
                                     (kill-buffer stdout))
                                   (delete-process stderr-pipe)
                                   (when (buffer-live-p stderr)
                                     (kill-buffer stderr))
                                   (when (buffer-live-p stderr-pipe-name)
                                     (kill-buffer stderr-pipe-name)))))

                   (aichat-debug "cur command: %s, config: \n%s" command config)
                   (with-current-buffer stdout
                     (setq-local aichat--curl-stderr stderr)
                     (setq-local aichat--curl-cleanup cleanup)
                     (setq-local aichat--curl-resolve resolve)
                     (setq-local aichat--curl-reject reject)

                     (setq-local aichat--curl-parser-state 'status)
                     (setq-local aichat--curl-parser-point (point-min))

                     (setq-local aichat--curl-response-status nil)
                     (setq-local aichat--curl-response-headers nil)
                     (setq-local aichat--curl-response-body nil)

                     (setq-local aichat--curl-response-callback callback)
                     (setq-local aichat--http-callback-data callback-data))

                   (condition-case err
                       (let ((proc (make-process
                                    :name program
                                    :buffer stdout
                                    :command command
                                    :stderr stderr-pipe
                                    :filter #'aichat--curl-process-filter
                                    :sentinel #'aichat--curl-process-sentinel)))
                         (process-send-string proc config)
                         (process-send-eof proc))
                     (error (funcall cleanup)
                            (signal (car err) (cdr err))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTTP API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun aichat-http (url &rest settings
                           &key
                           (backend nil)
                           (proxy nil)
                           (type nil)
                           (params nil)
                           (headers nil)
                           (data nil)
                           (callback-data nil)
                           (callback nil))
  "Request URL with property list SETTINGS as follow. Return value is promise,
the format of resolve value is (resp-status resp-headers resp-body).

===================== ======================================================
Keyword argument      Explanation
===================== ======================================================
BACKEND       (symbol)   curl or url
PROXY         (string)   proxy of current request
TYPE          (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS         (alist)   set \"?key=val\" part in URL
HEADERS        (alist)   additional headers to send with the request
DATA          (string)   data to be sent to the server
CALLBACK-DATA (object)   data to be used on CALLBACK by aichat--http-callack-data
CALLBACK      (string)   callbacl to receive reported http data.
"
  (let ((use-backend (if backend backend aichat-http-backend)))
    (if (eq use-backend 'curl)
        (apply #'aichat--curl-http url settings)
      (apply #'aichat--url-http url settings))))

(cl-defun aichat-http-event-source (url callback &rest settings
                                        &key
                                        (backend nil)
                                        (proxy nil)
                                        (type nil)
                                        (params nil)
                                        (headers nil)
                                        (data nil))
  "Request URL with property list SETTINGS as follow.

===================== ======================================================
Keyword argument      Explanation
===================== ======================================================
BACKEND       (symbol)   curl or url
PROXY         (string)   proxy of current request
TYPE          (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS         (alist)   set \"?key=val\" part in URL
HEADERS        (alist)   additional headers to send with the request
DATA          (string)   data to be sent to the server
"
  (promise-then (aichat-http url
                             :backend backend
                             :proxy proxy
                             :type type
                             :params params
                             :headers headers
                             :data data
                             :callback-data `((handle-data-p . t)
                                              (event-buffer . nil)
                                              (event-callback . ,callback))
                             :callback (lambda (status data)
                                         (aichat-debug "status: %s, handle: %s, data: \n%s" status (alist-get 'handle-data-p aichat--http-callback-data) data)
                                         (when (alist-get 'handle-data-p aichat--http-callback-data)
                                           (pcase status
                                             ('status
                                              (unless (string= "200" (car data))
                                                (setf (alist-get 'handle-data-p aichat--http-callback-data) nil)))
                                             ('headers
                                              (unless (string= "text/event-stream" 
                                                               (aichat-read-header-value "Content-Type" data))
                                                (setf (alist-get 'handle-data-p aichat--http-callback-data) nil)))
                                             ('body
                                              (let ((buffer (concat (alist-get 'event-buffer aichat--http-callback-data) data))
                                                    (start-pos 0)
                                                    (match-start))
                                                (while (setq match-start (string-match-p "\n\n" buffer start-pos))
                                                  (let ((lines (split-string (substring buffer start-pos match-start) "\n"))
                                                        (event-data nil)
                                                        (event-id nil))
                                                    (dolist (line lines)
                                                      (seq-let (prefix data) (split-string line ": ")
                                                        (cond
                                                         ((string= "data" prefix) (setq event-data (concat event-data data "\n")))
                                                         ((string= "event" prefix) (setq event-id data)))))
                                                    (when event-data
                                                      (funcall (alist-get 'event-callback aichat--http-callback-data) event-id (string-trim-right event-data))))
                                                  (setq start-pos (+ 2 match-start)))
                                                (setf (alist-get 'event-buffer aichat--http-callback-data) (substring buffer start-pos))))))))
                (lambda (value)
                  (promise-resolve value))
                (lambda (err)
                  (promise-reject err))))

(provide 'aichat-util)

;;; aichat-util.el ends here
