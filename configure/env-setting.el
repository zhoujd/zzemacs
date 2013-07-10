;;;; common-setting.el --- common config file
;;

;;add path for excute files
(if-ms-windows
 (setq my-env-path '(
                     ""
                     ))
 (setq my-env-path '(
                     ""
                     ))

(mapcar 'zz-add-os-path my-env-path)

(provide 'env-setting)

;;; env-setting.el ends here
