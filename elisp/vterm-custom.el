;;;; vterm-custom.el --- sample config file
;;;


(zz/load-path "site-lisp/emacs-vterm")

(require 'vterm)
(require 'multi-vterm)
(require 'vterm-toggle)

(setq vterm-tramp-shells
      '(
        ;; Use the default login shell for all TRAMP methods
        (t login-shell)
        ;; Example: Use a specific shell for the "docker" method
        ;; ("docker" "/bin/sh")
        ;; Example: Use login shell for ssh and scp, falling back to /bin/bash
        ;; (("ssh" login-shell "/bin/bash") ("scp" login-shell "/bin/bash"))
        ))

(defun zz/vterm-hook ()
  (defkeys-map vterm-mode-map
    ((kbd "C-c M-q") 'vterm-send-next-key)
    ((kbd "C-c M-o") 'vterm-clear))
  (defkeys-map vterm-copy-mode-map
    ((kbd "M-w") 'vterm-copy-mode-done)))

(add-hook 'vterm-mode-hook 'zz/vterm-hook)

(defun zz/get-vterm ()
  (interactive)
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory
                               (ido-read-directory-name "Directory: "))))
      (multi-vterm))))

(defun zz/home-vterm ()
  (interactive)
  (with-temp-buffer
    (let* ((default-directory "~"))
      (multi-vterm))))

(defun zz/remote-vterm ()
  "Open a remote vterm to a host"
  (interactive)
  (with-temp-buffer
    (let* ((default-directory (zz/get-host)))
      (when (file-exists-p default-directory)
        (call-interactively 'zz/get-vterm))
      )))


(provide 'vterm-custom)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-custom.el ends here
