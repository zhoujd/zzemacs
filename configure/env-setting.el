;;;; env-setting.el --- env config file
;;

;;add path for excute files
(when-ms-windows
 (setq my-env-path (list
                    (concat zzemacs-path "bin")
                    "C:/Program Files (x86)/Mozilla Firefox"
                    "C:/Program Files (x86)/Beyond Compare 3"
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
