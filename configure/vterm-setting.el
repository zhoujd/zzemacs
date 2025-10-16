;;;; vterm-setting.el --- sample config file
;;;

;;vterm
(when module-file-suffix
  (zz/load-path "site-lisp/emacs-vterm")
  (require 'vterm)
  (require 'multi-vterm)
  (require 'vterm-toggle)
  (defkeys-map ctrl-z-map
    ((kbd "v") vterm-mode-map)))


(provide 'vterm-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-setting.el ends here
