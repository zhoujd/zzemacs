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
     "C:/strawberry/perl/bin"
     "C:/strawberry/c/bin"
     "C:/Vim/vim73"
     "C:/Program Files (x86)/CMake 2.8/bin"
     "C:/Program Files (x86)/Mozilla Firefox"
     "C:/Program Files (x86)/Microsoft Visual Studio 10.0/VC/bin"
     "C:/Program Files (x86)/Microsoft Visual Studio 10.0/VC"
     "C:/Program Files (x86)/Microsoft Visual Studio 10.0/Common7/IDE"
     "C:/Program Files/Windows Kits/8.0/Debuggers/x64"
     "C:/Scheme/Gambit-C/v4.6.7/bin"
     "C:/Scheme/Racket"
     "C:/Python27/Scripts"
     "C:/Python27"
     "C:/Lisp/ecl"
     "C:/Lisp/SBCL"
     "C:/Lisp/clisp"
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

(mapcar 'zz-add-os-path (reverse my-env-path))

(provide 'env-setting)

;;; env-setting.el ends here
