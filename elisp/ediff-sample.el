;;; ediff sample file for git/hg diff-merge
;;;

;; UI setting
(if (fboundp 'menu-bar-mode)     (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'tool-bar-mode)     (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)   (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode)      (tooltip-mode -1))

;; Set default-frame-alist
(if window-system
    (setq default-frame-alist
          (append
           '((scroll-bar-width . 16)
             (width . 140)
             (height . 36))
           default-frame-alist)))

;; Color setting
(set-face-background 'default "black")
(set-face-foreground 'default "white")

;; Set ediff style
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun ediff-sample-diff (diff-a diff-b)
  (interactive)
  (if (or (file-directory-p diff-a)
          (file-directory-p diff-b))
      (ediff-sample-dirs diff-a diff-b)
      (ediff-sample-files diff-a diff-b)))

(defun ediff-sample-files (file-a file-b)
  (setq ediff-quit-hook 'save-buffers-kill-emacs)
  (ediff-files file-a file-b))

(defun ediff-sample-dirs (dir-a dir-b)
  (setq ediff-quit-session-group-hook 'save-buffers-kill-emacs)
  (ediff-directories dir-a dir-b ""))

(defun ediff-merge-files (local remote base target)
  (setq ediff-quit-hook 'save-buffers-kill-emacs)
  (ediff-merge-files-with-ancestor remote base local nil target))


;;quick move other windows
(global-set-key [M-up]          'windmove-up)
(global-set-key [M-down]        'windmove-down)
(global-set-key [M-right]       'windmove-right)
(global-set-key [M-left]        'windmove-left)

;;window size change
(global-set-key [S-up]          'enlarge-window)
(global-set-key [S-down]        'shrink-window)
(global-set-key [S-right]       'enlarge-window-horizontally)
(global-set-key [S-left]        'shrink-window-horizontally)


(provide 'ediff-sample)

;;; ediff-sample.el ends here
