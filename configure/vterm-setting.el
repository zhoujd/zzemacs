;;;; vterm-setting.el --- sample config file
;;;

;;vterm
(when module-file-suffix
  (zz/load-path "site-lisp/emacs-vterm")
  (require 'vterm)
  (require 'multi-vterm)
  (require 'vterm-toggle)
  (defun zz/vterm-hook ()
    (defkeys-map vterm-mode-map
      ((kbd "C-c M-o") 'vterm-clear)))
  (add-hook 'vterm-mode-hook 'zz/vterm-hook)
  (message "Enable vterm...done"))


(provide 'vterm-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-setting.el ends here
