;;;; eshell-setting.el --- sample config file
;;;

(require 'eshell)
(require 'em-smart)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         "[" (user-login-name) "@" (system-name) " "
         (if (string= (eshell/pwd) (getenv "HOME"))
             "~" (eshell/basename (eshell/pwd)))
         "]"
         (if (= (user-uid) 0) "# " "$ "))))

;clear the eshell buffer
(defun zz:eshell-clear ()
  (interactive)
  (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))

(defun zz:eshell-hook ()
  (local-set-key (kbd "C-c M-o") 'zz:eshell-clear))
(add-hook 'eshell-mode-hook 'zz:eshell-hook)


(provide 'eshell-setting)

;;; eshell-setting.el ends here
