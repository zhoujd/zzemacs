;;;; env-setting.el --- env config file
;;

(defun zz:add-os-env (path name)
  "add path to env name"
  (when (file-exists-p path)
    (let ((env (getenv name)))
      (when (or (not env)
                (not (string-match path env)))
        (setenv name (concat path path-separator env))))))

(defun zz:add-os-path (path)
  "add path to PATH"
  (interactive "DDirectory: ")
  (zz:add-os-env path "PATH")
  (setq exec-path (cons path exec-path)))

(defun zz:add-lib-path (path)
  "add path to LD_LIBRARY_PATH"
  (interactive "DDirectory: ")
  (zz:add-os-env path "LD_LIBRARY_PATH"))

(defun zz:add-pkg-path (path)
  "add path to PKG_CONFIG_PATH"
  (interactive "DDirectory: ")
  (zz:add-os-env path "PKG_CONFIG_PATH"))

(defvar zz:env-path
  (if-ms-windows
   (progn
    (list
     (format "%s/bin" (getenv "EMACS_DIR"))
     (format "%s/bin" zzemacs-path)
     (format "%s/libexec" zzemacs-path)
     "C:/Git/bin"
     "C:/Git/usr/bin"
     "C:/strawberry/perl/bin"
     "C:/strawberry/c/bin"
     "C:/lisp/ecl"
     "C:/lisp/SBCL"
     "C:/lisp/clisp"
     ))
   (progn
    (list
     (format "%s/bin" zzemacs-path)
     (format "%s/libexec" zzemacs-path)
     )))
  "add to path and exec-path")

(mapc #'zz:add-os-path zz:env-path)

(defun zz:add-os-proxy (proxy)
  "add url proxy"
  (interactive "sProxy: ")
  (setq url-proxy-services
        `(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
          ("http" . ,proxy)
          ("https" . ,proxy))))

(defun zz:trim-address (address)
  "Trim proxy ADDRESS from '<scheme>://<host>:<port>' into '<host>:<port>'"
  (if (stringp address)
      (car (last (split-string address "//")))
      address))

(defun zz:use-os-proxy ()
  "Use system environment proxy"
  (interactive)
  (let ((http_proxy (zz:trim-address (getenv "HTTP_PROXY")))
        (https_proxy (zz:trim-address (getenv "HTTPS_PROXY")))
        (no_proxy (getenv "NO_PROXY")))
    (when (and (> (length http_proxy) 0)
               (> (length https_proxy) 0))
      (setq url-proxy-services
            `(("no_proxy" . ,no_proxy)
              ("http" . ,http_proxy)
              ("https" . ,https_proxy)))
      (message "Use OS proxy: %s" http_proxy))))

(zz:use-os-proxy)

(setenv "EDITOR" "emacsclient -t")
(setenv "VISUAL" "emacsclient -c -a emacs")


(provide 'env-setting)

;;; env-setting.el ends here
