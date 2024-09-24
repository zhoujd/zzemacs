;;;; http-setting.el --- http setting file
;;

(zz/load-path "site-lisp/restclient")
(zz/load-path "site-lisp/know-your-http-well")
(require 'company-restclient)

;;eg: (princ (url-http-get "http://httpbin.org/get" nil))
(defun url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let (
        (response-string nil)
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (switch-to-buffer
     (url-retrieve-synchronously url))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (setq response-string
          (buffer-substring-no-properties (point) (point-max)))
    (kill-buffer (current-buffer))
    response-string))

;;eg: (princ (url-http-post "http://httpbin.org/post" '(("use" . "dummy")  ("pass" . "something else") ("code" . "pxyz0011213"))))
(defun url-http-get (url args)
  "Send ARGS to URL as a GET request."
  (let (
        (response-string nil)
        (url-request-method "GET")
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (switch-to-buffer
     (url-retrieve-synchronously
      (concat url "?" url-request-data)))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (setq response-string
          (buffer-substring-no-properties
           (point) (point-max)))
    (kill-buffer (current-buffer))
    response-string))

(defvar helm-httpstatus-source
  '((name . "HTTP STATUS")
    (candidates . (("100 Continue") ("101 Switching Protocols")
                   ("102 Processing") ("200 OK")
                   ("201 Created") ("202 Accepted")
                   ("203 Non-Authoritative Information") ("204 No Content")
                   ("205 Reset Content") ("206 Partial Content")
                   ("207 Multi-Status") ("208 Already Reported")
                   ("300 Multiple Choices") ("301 Moved Permanently")
                   ("302 Found") ("303 See Other")
                   ("304 Not Modified") ("305 Use Proxy")
                   ("307 Temporary Redirect") ("400 Bad Request")
                   ("401 Unauthorized") ("402 Payment Required")
                   ("403 Forbidden") ("404 Not Found")
                   ("405 Method Not Allowed") ("406 Not Acceptable")
                   ("407 Proxy Authentication Required") ("408 Request Timeout")
                   ("409 Conflict") ("410 Gone")
                   ("411 Length Required") ("412 Precondition Failed")
                   ("413 Request Entity Too Large")
                   ("414 Request-URI Too Large")
                   ("415 Unsupported Media Type")
                   ("416 Request Range Not Satisfiable")
                   ("417 Expectation Failed") ("418 I'm a teapot")
                   ("421 Misdirected Request")
                   ("422 Unprocessable Entity") ("423 Locked")
                   ("424 Failed Dependency") ("425 No code")
                   ("426 Upgrade Required") ("428 Precondition Required")
                   ("429 Too Many Requests")
                   ("431 Request Header Fields Too Large")
                   ("449 Retry with") ("500 Internal Server Error")
                   ("501 Not Implemented") ("502 Bad Gateway")
                   ("503 Service Unavailable") ("504 Gateway Timeout")
                   ("505 HTTP Version Not Supported")
                   ("506 Variant Also Negotiates")
                   ("507 Insufficient Storage") ("509 Bandwidth Limit Exceeded")
                   ("510 Not Extended")
                   ("511 Network Authentication Required")))
    (action . message)))

(defvar helm-clj-http-source
  '((name . "clj-http options")
    (candidates
     .
     ((":accept - keyword for content type to accept")
      (":as - output coercion: :json, :json-string-keys, :clojure, :stream, :auto or string")
      (":basic-auth - string or vector of basic auth creds")
      (":body - body of request")
      (":body-encoding - encoding type for body string")
      (":client-params - apache http client params")
      (":coerce - when to coerce response body: :always, :unexceptional, :exceptional")
      (":conn-timeout - timeout for connection")
      (":connection-manager - connection pooling manager")
      (":content-type - content-type for request")
      (":cookie-store - CookieStore object to store/retrieve cookies")
      (":cookies - map of cookie name to cookie map")
      (":debug - boolean to print info to stdout")
      (":debug-body - boolean to print body debug info to stdout")
      (":decode-body-headers - automatically decode body headers")
      (":decompress-body - whether to decompress body automatically")
      (":digest-auth - vector of digest authentication")
      (":follow-redirects - boolean whether to follow HTTP redirects")
      (":form-params - map of form parameters to send")
      (":headers - map of headers")
      (":ignore-unknown-host? - whether to ignore inability to resolve host")
      (":insecure? - boolean whether to accept invalid SSL certs")
      (":json-opts - map of json options to be used for form params")
      (":keystore - file path to SSL keystore")
      (":keystore-pass - password for keystore")
      (":keystore-type - type of SSL keystore")
      (":length - manually specified length of body")
      (":max-redirects - maximum number of redirects to follow")
      (":multipart - vector of multipart options")
      (":oauth-token - oauth token")
      (":proxy-host - hostname of proxy server")
      (":proxy-ignore-hosts - set of hosts to ignore for proxy")
      (":proxy-post - port for proxy server")
      (":query-params - map of query parameters")
      (":raw-headers - boolean whether to return raw headers with response")
      (":response-interceptor - function called for each redirect")
      (":retry-handler - function to handle HTTP retries on IOException")
      (":save-request? - boolean to return original request with response")
      (":socket-timeout - timeout for establishing socket")
      (":throw-entire-message? - whether to throw the entire response on errors")
      (":throw-exceptions - boolean whether to throw exceptions on 5xx & 4xx")
      (":trust-store - file path to trust store")
      (":trust-store-pass - password for trust store")
      (":trust-store-type - type of trust store")))
    (action . message)))

(defun helm-httpstatus ()
  (interactive)
  (helm-other-buffer '(helm-httpstatus-source) "*helm httpstatus*"))

(defun helm-clj-http ()
  (interactive)
  (helm-other-buffer '(helm-clj-http-source) "*helm clj-http flags*"))


(provide 'http-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; http-setting.el ends here
