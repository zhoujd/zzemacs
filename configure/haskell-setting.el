;;;;hask programme setting


;;haskell setting
(zz/load-path "site-lisp/haskell-mode")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list (concat zzemacs-path "/site-lisp/haskell-mode/"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
(add-hook 'haskell-mode-hook 'outline-minor-mode)

(defun zz/haskell-mode-hook()
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "^//w+[^/n/r]*::[^/n/r]*"))

(add-hook 'haskell-mode-hook 'zz/haskell-mode-hook)

(provide 'haskell-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; haskell-setting.el end here
