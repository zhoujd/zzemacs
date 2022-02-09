;;;; env-setting.el --- env config file
;;

;;use bash as default for emacs27
(unless (< emacs-major-version 27)
  (setenv "SHELL" "/bin/bash"))

;;add path to env name
(defun zz:add-os-env (path name)
  (when (file-exists-p path)
    (let ((env (getenv name)))
      (when (or (not env)
                (not (string-match path env)))
        (setenv name (concat path path-separator env))
        ))))

;;add path to PATH
(defun zz:add-os-path (path)
  (interactive "DDirectory: ")
  (zz:add-os-env path "PATH")
  (setq exec-path (cons path exec-path)))

;;add path to LD_LIBRARY_PATH
(defun zz:add-lib-path (path)
  (interactive "DDirectory: ")
  (zz:add-os-env path "LD_LIBRARY_PATH"))

;;add path to PKG_CONFIG_PATH
(defun zz:add-pkg-path (path)
  (interactive "DDirectory: ")
  (zz:add-os-env path "PKG_CONFIG_PATH"))

;;add url proxy
(defun zz:add-os-proxy (proxy)
  (interactive "sProxy: ")
  (setq url-proxy-services
        `(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
          ("http" . ,proxy)
          ("https" . ,proxy))))

;;add path for excute files
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

(provide 'env-setting)

;;; env-setting.el ends here
