;;;; env-setting.el --- env config file
;;

;;add path for excute files
(when-ms-windows
 (setq my-env-path (list
                    (concat zzemacs-path "bin")
                    "C:/zznix/bin"
                    (format "%s/bin" (getenv "EMACS_DIR"))
                    (format "%s/zztools/bcompare" (getenv "HOME"))
                    (format "%s/zztools/perforce" (getenv "HOME"))
                    "C:/strawberry/perl/bin"
                    "C:/strawberry/c/bin"
                    "C:/Git/bin"
                    "C:/Git/libexec/git-core"
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
                    )))
(unless-ms-windows
 (setq my-env-path (list
                    (concat zzemacs-path "bin")
                    "/usr/racket/bin"
                    "/usr/local/Gambit-C/bin/"
                    (format "%s/zztools/bcompare/bin" (getenv "HOME"))
                    (format "%s/zztools/meld/bin" (getenv "HOME"))
                    (format "%s/zztools/p4v/bin" (getenv "HOME"))
                    )))

(mapcar 'zz-add-os-path (reverse my-env-path))

(provide 'env-setting)

;;; env-setting.el ends here
