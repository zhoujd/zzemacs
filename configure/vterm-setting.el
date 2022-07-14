;;;; vterm-setting.el --- sample config file
;;;

;;vterm
(when module-file-suffix
  (zz:load-path "site-lisp/emacs-libvterm")
  (require 'vterm)
  (require 'multi-vterm)
  (require 'vterm-toggle))


(provide 'vterm-setting)

;;; vterm-setting.el ends here
