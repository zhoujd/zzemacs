;;; ediff sample file for git/hg diff-merge
;;;

;; UI setting
(if (fboundp 'menu-bar-mode)     (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'tool-bar-mode)     (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)   (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode)      (tooltip-mode -1))

;; Color setting
(set-face-background 'default "black")
(set-face-foreground 'default "white")

;; Set ediff style
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun ediff-sample-files (file-a file-b)
  (setq ediff-quit-hook 'save-buffers-kill-emacs)
  (ediff-files file-a file-b))

(defun ediff-sample-dirs (dir-a dir-b)
  (ediff-directories dir-a dir-b ""))

(defun ediff-merge-files (local remote base target)
  (setq ediff-quit-hook 'save-buffers-kill-emacs)
  (ediff-merge-files-with-ancestor remote base local nil target))

(provide 'ediff-sample)

;;; ediff-sample.el ends here
