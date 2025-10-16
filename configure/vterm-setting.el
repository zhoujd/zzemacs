;;;; vterm-setting.el --- sample config file
;;;

;;vterm
(zz/load-path "elisp")

(when module-file-suffix
  (require 'vterm-custom)
  (message "Enable vterm...done"))


(provide 'vterm-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-setting.el ends here
