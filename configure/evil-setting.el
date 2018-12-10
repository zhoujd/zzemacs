;;;; evil-setting.el --- evil setting file
;;;

;;http://emacswiki.org/emacs/Evil
(zz-load-path "site-lisp/evil")
(zz-load-path "site-lisp/evil/lib")
(zz-load-path "site-lisp/evil-plugin")

;;evil-evil
(require 'evil)

;;toggle evil mode
(defun zz:toggle-evil-mode ()
  (interactive)
  (if (bound-and-true-p evil-local-mode)
      (progn
       ;;go emacs
       (evil-local-mode (or -1 1))
       (undo-tree-mode (or -1 1))
       (set-variable 'cursor-type 'box))
      (progn
       ;;go evil
       (evil-local-mode (or 1 1))
       (evil-refresh-cursor))))

;;evil-surround
(require 'evil-surround)
(global-evil-surround-mode t)

;;evil-leader
(require 'evil-leader)
(global-evil-leader-mode 1)
(evil-leader/set-key "e" 'find-file
                     "b" 'switch-to-buffer
                     "k" 'kill-buffer)

;;disable undo-tree on mode-line
(setq undo-tree-mode-lighter "")


(provide 'evil-setting)

;;; evil-setting.el ends here
