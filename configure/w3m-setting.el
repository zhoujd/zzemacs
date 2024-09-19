;;;; w3m-setting.el --- w3m setting file
;;;http://emacs-w3m.namazu.org/index-en.html
;;;https://github.com/emacs-w3m/emacs-w3m
;;;https://melpa.org/#/helm-w3m

(zz/load-path "site-lisp/w3m")
(require 'w3m-load)

(defvar zz/w3m-path (concat zzemacs-path "/site-lisp/w3m")
  "emacs-w3m path")

;;build w3m
(defun zz/w3m-build-in-emacs ()
  "compile emacs-w3m"
  (interactive)
  (apply
   'start-process
   "my w3m compile"
   nil
   (split-string
    (format "%s -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install"
            (if-ms-windows (concat (getenv "EMACS_DIR") "/bin/emacs") "emacs"))))
  (message "build emacs-w3m ok"))

;;browser selecting
(setq browse-url-browser-function
      (list (cons (concat zzemacs-path "/doc/hyperspec/") 'w3m-browse-url)
            (cons "." 'browse-url-default-browser)))

;;allow browsing of local files /usr/lib/w3m/cgi-bin/dirlist.cgi
(setq w3m-dirlist-cgi-program (concat zz/w3m-path "/cgi-bin/dirlist.cgi"))

;;causes the return key to submit a form
(setq w3m-use-form t)
(setq w3m-use-mule-ucs t)
(setq w3m-use-toolbar t)  
(setq w3m-use-cookies t)

;;show images: Press "T"
(setq w3m-toggle-inline-image t)
(setq w3m-tab-width 4)
(setq w3m-fill-column 120)
(setq w3m-home-page "http://www.google.com/")
(setq w3m-view-this-url-new-session-in-background t)

;;icons path
(setq w3m-icon-directory (concat zz/w3m-path "/icons"))

;;http_proxy and no_proxy
(defun zz/w3m-os-proxy ()
  (interactive)  
  (let ((http_proxy (getenv "HTTP_PROXY"))
        (no_proxy (getenv "NO_PROXY")))
    (when (> (length http_proxy) 0)
      (setq w3m-command-arguments
            (nconc w3m-command-arguments
                   `("-o" ,(concat "http_proxy=" http_proxy))))
      (message "w3m use os http_proxy"))
    (when (> (length no_proxy) 0)
      (setq w3m-no-proxy-domains (split-string no_proxy ","))
      (message "w3m use os no_proxy"))))


(provide 'w3m-setting)

;;;; w3m-setting.el ends here
