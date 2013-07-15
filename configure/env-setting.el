;;;; env-setting.el --- env config file
;;

;;add path for excute files
(when-ms-windows
 (setq my-env-path (list
                    (concat zzemacs-path "bin")
                    "C:/zznix/bin"
                    "C:/emacs-23.4/bin"
                    "C:/Git/bin"
                    "C:/Vim/vim73"
                    "C:/Program Files (x86)/Mozilla Firefox"
                    "C:/Program Files (x86)/Beyond Compare 3"
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
                    )))

(mapcar 'zz-add-os-path my-env-path)

(provide 'env-setting)

;;; env-setting.el ends here
