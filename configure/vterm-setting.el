;;;; vterm-setting.el --- sample config file
;;;

;;vterm
(zz:load-path "site-lisp/emacs-libvterm")
(require 'vterm)
(require 'multi-vterm)
(require 'vterm-toggle)

(defun zz:cd-vterm ()
  (interactive)
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory
                               (ido-read-directory-name "Directory: ")))
           (multi-vterm-default-dir default-directory))
      (multi-vterm))))

(defun zz:local-vterm ()
  (interactive)
  (with-temp-buffer
    (when (tramp-tramp-file-p default-directory)
      (setq default-directory "~"))
    (let* ((default-directory (file-name-as-directory
                               (ido-read-directory-name "Directory: ")))
           (multi-vterm-default-dir default-directory))
      (multi-vterm))))

(defun zz:remote-vterm (host)
  "Connect to a remote host by multi-vterm."
  (interactive "sHost: ")
  (with-temp-buffer
    (let* ((multi-vterm-program "ssh")
           (multi-vterm-program-switches (format "%s" host))
           (default-directory "~"))
      (multi-vterm)
      (setq default-directory (format "/%s:%s:" tramp-default-method  host))
      )))

(defun zz:helm-cd-vterm (dir)
  (interactive "DDirectory: ")
  (with-temp-buffer
    (let* ((default-directory dir)
           (multi-vterm-default-dir default-directory))
      (multi-vterm))))

(defun zz:helm-local-vterm ()
  "remote term with vhelm"
  (interactive)
  (with-temp-buffer
    (let* ((prefix "~"))
      (when (tramp-tramp-file-p default-directory)
        (setq default-directory prefix))
      (call-interactively 'zz:helm-cd-vterm)
    )))

(defun zz:helm-remote-vterm ()
  "remote vterm with helm"
  (interactive)
  (with-temp-buffer
    (let* ((prefix (concat "/" tramp-default-method ":")))
      (unless (tramp-tramp-file-p default-directory)
        (setq default-directory prefix))
      (call-interactively 'zz:helm-cd-vterm)
    )))


(provide 'vterm-setting)

;;; vterm-setting.el ends here
