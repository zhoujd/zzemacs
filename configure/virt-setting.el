;;;; virt-setting.el

(unless-ms-windows
 (zz/load-path "site-lisp/vagrant-tramp")
 (require 'vagrant-tramp)
 (require 'vagrant))

;;virt manager
(require 'virt-manager)


(provide 'virt-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; virt-setting.el --- end here
