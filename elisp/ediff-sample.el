;;; ediff sample file for git/hg diff-merge
;;;


;; Set ediff style
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun ediff-sample-files (file-a file-b)
  (setq ediff-quit-hook 'kill-emacs)
  (ediff-files file-a file-b))

(defun ediff-merge-files (local remote base target)
  (setq ediff-quit-hook 'kill-emacs)
  (ediff-merge-files-with-ancestor remote base local nil target))

(provide 'ediff-sample)

;;; ediff-sample.el ends here
