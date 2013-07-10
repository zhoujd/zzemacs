;;;; common-setting.el --- common config file
;;

;;add path for excute files
(when-ms-windows
 (setq my-env-path '(
                     "C:/Program Files (x86)/Mozilla Firefox/"
                     )))
(unless-ms-windows
 (setq my-env-path '(
                     ""
                     )))

(mapcar 'zz-add-os-path my-env-path)

(provide 'env-setting)

;;; env-setting.el ends here
