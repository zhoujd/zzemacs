;;;; env-setting.el --- env config file
;;

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

;;add path for excute files
(defvar zz:env-path
  (if-ms-windows
   (progn
    (list
     (format "%s/bin" (getenv "EMACS_DIR"))
     (format "%s/bin" zzemacs-path)
     (format "%s/libexec" zzemacs-path)
     "C:/aspell/bin"
     "C:/strawberry/perl/bin"
     "C:/strawberry/c/bin"
     "C:/lisp/ecl"
     "C:/lisp/SBCL"
     "C:/lisp/clisp"
     ))
   (progn
    (list
     (format "%s/local/bin" (getenv "HOME"))
     (format "%s/bin" zzemacs-path)
     (format "%s/libexec" zzemacs-path)
     )))
  "add to path and exec-path")

(mapc #'zz:add-os-path zz:env-path)

(provide 'env-setting)

;;; env-setting.el ends here
