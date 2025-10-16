;;;; vterm-custom.el --- sample config file
;;;


(zz/load-path "site-lisp/emacs-vterm")

(require 'vterm)
(require 'multi-vterm)
(require 'vterm-toggle)

(defun zz/vterm-hook ()
  (defkeys-map global-map
    ((kbd "C-c v") vterm-mode-map))
  (defkeys-map vterm-mode-map
    ((kbd "C-c M-o") 'vterm-clear))
  (defkeys-map vterm-copy-mode-map
    ((kbd "M-w") 'vterm-copy-mode-done)))

(add-hook 'vterm-mode-hook 'zz/vterm-hook)


(provide 'vterm-custom)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-custom.el ends here
