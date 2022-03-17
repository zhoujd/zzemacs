;;;; virt-setting.el

(unless-ms-windows
 (zz:load-path "site-lisp/vagrant-tramp")
 (require 'vagrant-tramp)
 (require 'vagrant))

;;virt manager
(require 'virt-manager)


(provide 'virt-setting)

;;;; virt-setting.el --- end here
