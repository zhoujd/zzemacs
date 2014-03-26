;;;; ediff-setting.el --- common config file
;;

(require 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq git-mergetool-emacsclient-ediff-active nil)

;; Set ediff style
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration (current-frame-configuration))
  (setq local-ediff-saved-window-configuration (current-window-configuration))
  ;; (local-ediff-frame-maximize)
  (if git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

;; Setup hook for ediff
(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook         'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook      'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun git-mergetool-emacsclient-ediff (local remote base merged)
  (setq git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun git-mergetool-emacsclient-ediff-after-quit-hook ()
  (if git-mergetool-emacsclient-ediff-active
      (exit-recursive-edit)))

(add-hook 'ediff-after-quit-hooks 'git-mergetool-emacsclient-ediff-after-quit-hook 'append)

;; Clean up when ediff quit
(defvar ediff-temp-file "" "remember temp file name")
(defvar emerge-temp-local-file "")
(defvar emerge-temp-base-file "")
(defvar emerge-temp-remote-file "")

(defun local-get-buffer-name (file-name)
  (car (reverse (split-string file-name "/"))))

(defun local-ediff-clean-up ()
  (mapc (lambda (name)
          (when (get-buffer name)
            (kill-buffer name)))
        (list
         "*Ediff Control Panel*"
         "*Ediff Registry*"
         "*ediff-diff*"
         "*ediff-fine-diff*"
         "*ediff-errors*"
         (local-get-buffer-name ediff-temp-file)
         (local-get-buffer-name emerge-temp-base-file)
         (local-get-buffer-name emerge-temp-local-file)
         (local-get-buffer-name emerge-temp-remote-file)))
  (setq ediff-temp-file "")
  (setq emerge-temp-base-file "")
  (setq emerge-temp-local-file "")
  (setq emerge-temp-remote-file "")
  (delete-frame))

(add-hook 'ediff-quit-hook 'local-ediff-clean-up)

(provide 'ediff-setting)

;;; ediff-setting.el ends here
