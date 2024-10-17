;;;; eshell-setting.el --- sample config file
;;;

(require 'eshell)
(require 'em-smart)

(setq eshell-banner-message "")
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

(defun zz/get-eshell ()
  (interactive)
  (let ((default-directory (file-name-as-directory
                            (ido-read-directory-name "Directory: "))))
    (when (file-exists-p default-directory)
      (eshell))))

(defun zz/switch-to-eshell ()
  (interactive)
  (let ((buf-name "*eshell*"))
    (if (get-buffer buf-name)
        (switch-to-buffer buf-name)
        (zz/get-eshell))))

(defun zz/eshell-clear ()
  (interactive)
  (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))

(defun zz/eshell-hook ()
  (local-set-key (kbd "C-c M-o") 'zz/eshell-clear))
(add-hook 'eshell-mode-hook 'zz/eshell-hook)

(defun zz/eshell-add-aliases ()
  "Add eshell aliases"
  (dolist (var '(("ff" "find-file $1")
                 ("ll" "ls -hal $*")))
    (add-to-list 'eshell-command-aliases-list var)))
(add-hook 'eshell-post-command-hook 'zz/eshell-add-aliases)


(provide 'eshell-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; eshell-setting.el ends here
