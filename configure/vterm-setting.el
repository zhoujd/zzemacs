;;;; vterm-setting.el --- sample config file
;;;

;;vterm
(when module-file-suffix
  (zz/load-path "site-lisp/emacs-vterm")
  (require 'vterm)
  (require 'multi-vterm)
  (require 'vterm-toggle)
  (defun zz/vterm-hook ()
    (local-set-key (kbd "C-c v") vterm-mode-map)
    (local-set-key (kbd "C-c M-o") 'vterm-clear))
  (add-hook 'vterm-mode-hook 'zz/vterm-hook)
  (message "Enable vterm...done"))


(provide 'vterm-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-setting.el ends here
