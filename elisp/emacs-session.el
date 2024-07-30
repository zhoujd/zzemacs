;;; emacs session

;; session + desktop
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file-coding-system 'utf-8)
(when (< emacs-major-version 27)
  (desktop-load-default))

;; use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(setq desktop-load-locked-desktop t)

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
      (lambda ()
        ;; desktop-remove clears desktop-dirname
        (setq desktop-dirname-tmp desktop-dirname)
        (desktop-remove)
        (setq desktop-dirname desktop-dirname-tmp)))

(defun zz:session-saved-p ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; restore the desktop manually
(defun zz:session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (zz:session-saved-p)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun zz:session-save ()
  "Save an emacs session."
  (interactive)
  (if (zz:session-saved-p)
      (if (y-or-n-p "Overwrite existing desktop? ")
      (desktop-save-in-desktop-dir)
    (message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
          (lambda ()
            (if (zz:session-saved-p)
                (if (y-or-n-p "Restore desktop? ")
                    (zz:session-restore)))))

;;save desktop when exit
(defvar zz:session-exit-p nil "t for enable, nil for disable")
(when zz:session-exit-p
  (add-hook 'kill-emacs-hook (lambda () (zz:session-save))))


(provide 'emacs-session)

;;; emacs-session.el ends here
