;;;; evil-setting.el --- evil setting file
;;;

;;http://emacswiki.org/emacs/Evil
(zz-load-path "site-lisp/evil")
(zz-load-path "site-lisp/evil/lib")
(zz-load-path "site-lisp/evil-plugin")

;;evil-evil
(require 'evil)

;;evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;;evil-leader
(require 'evil-leader)
(global-evil-leader-mode 1)
(evil-leader/set-key "e" 'find-file
                     "b" 'switch-to-buffer
                     "k" 'kill-buffer)

;;key setting using default
;(evil-set-toggle-key "<f5>")

(provide 'evil-setting)

;;; evil-setting.el ends here
