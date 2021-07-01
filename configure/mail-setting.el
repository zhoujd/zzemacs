;;;; mail-setting.el --- mail setting file
;;

;;EWS
(zz:load-path "site-lisp/excorporate")
(require 'excorporate)
(setq excorporate-configuration '("zachary.zhou@intel.com" . "https://outlook.office365.com/EWS/Exchange.asmx"))


(provide 'mail-setting)

;;; mail-setting.el ends here
