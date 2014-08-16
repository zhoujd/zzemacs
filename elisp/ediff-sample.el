;;; ediff sample file for git/hg diff-merge
;;;

(defun ediff-sample-files (file-a file-b)
  (setq ediff-quit-hook 'kill-emacs)
  (ediff-files file-a file-b))

(provide 'ediff-sample)

;;; ediff-sample.el ends here
