;;;; env-setting.el --- env config file
;;

;;add path to PATH
(defun my:add-os-path (path)
  (interactive "DDirectory: ")
  (when (file-exists-p path)
    (unless (string-match path (getenv "PATH"))
      (setenv "PATH" (concat path path-separator (getenv "PATH")))
      (setq exec-path (cons path exec-path)))))

;;add path to LD_LIBRARY_PATH
(defun my:add-lib-path (path)
  (interactive "DDirectory: ")
  (when (file-exists-p path)
    (unless (string-match path (getenv "LD_LIBRARY_PATH"))
      (setenv "LD_LIBRARY_PATH" (concat path
                                        path-separator
                                        (getenv "LD_LIBRARY_PATH"))))))

;;add path for excute files
(defvar my:env-path
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

(mapc #'my:add-os-path my:env-path)

(provide 'env-setting)

;;; env-setting.el ends here
