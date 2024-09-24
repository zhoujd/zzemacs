;;;; vterm-setting.el --- sample config file
;;;

;;vterm
(when module-file-suffix
  (zz/load-path "site-lisp/emacs-vterm")
  (require 'vterm)
  (require 'multi-vterm)
  (require 'vterm-toggle))


(provide 'vterm-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-setting.el ends here
