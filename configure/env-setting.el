;;;; env-setting.el --- env config file
;;

;;add path for excute files
(defvar my-env-path
  (if-ms-windows
   (progn
    (list
     (format "%s/bin" (getenv "ZZNIX_HOME"))
     (format "%s/libexec/emacs" (getenv "ZZNIX_HOME"))
     (format "%s/bin" zzemacs-path)
     (format "%s/libexec" zzemacs-path)
     (format "%s/bin" (getenv "EMACS_DIR"))
     "C:/aspell/bin"
     "C:/strawberry/perl/bin"
     "C:/strawberry/c/bin"
     "C:/scheme/Gambit-C/v4.6.7/bin"
     "C:/scheme/Racket"
     "C:/python27/Scripts"
     "C:/python27"
     "C:/lisp/ecl"
     "C:/lisp/SBCL"
     "C:/lisp/clisp"
     ))
   (progn
    (list
     "/sbin"
     "/usr/sbin"
     "/usr/local/racket/bin"
     "/usr/local/Gambit-C/bin"
     (format "%s/bin" zzemacs-path)
     (format "%s/libexec" zzemacs-path)
     (format "%s/zzmeld/meld/bin" (getenv "HOME"))
     (format "%s/zzp4merge/p4v/bin" (getenv "HOME"))
     )))
  "add to path and exec-path")

(mapc #'zz-add-os-path my-env-path)

(provide 'env-setting)

;;; env-setting.el ends here
