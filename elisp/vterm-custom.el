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

(defvar zz/vterm-map (make-sparse-keymap) "Custom vterm-map")
(defkeys-map zz/vterm-map
  ((kbd "c") 'multi-vterm)
  ((kbd "n") 'multi-vterm-next)
  ((kbd "p") 'multi-vterm-prev)
  ((kbd "s") 'zz/switch-to-vterm)
  ((kbd "r") 'zz/remote-vterm)
  ((kbd "h") 'zz/home-vterm))

(defun zz/vterm-hook ()
  (defkeys-map vterm-mode-map
    ((kbd "C-c M-q") 'vterm-send-next-key)
    ((kbd "C-c M-o") 'vterm-clear)
    ((kbd "C-c M-n") 'multi-vterm-next)
    ((kbd "C-c M-p") 'multi-vterm-prev))
  (defkeys-map vterm-copy-mode-map
    ((kbd "M-w") 'vterm-copy-mode-done)))

(add-hook 'vterm-mode-hook 'zz/vterm-hook)

(defun zz/get-vterm ()
  (interactive)
  (let ((dir (file-name-as-directory (ido-read-directory-name "Directory: "))))
    (let ((default-directory dir))
      (multi-vterm))
    (delete-other-windows)))

(defun zz/home-vterm ()
  (interactive)
  (let ((default-directory (expand-file-name "~")))
    (multi-vterm)
    (delete-other-windows)))

(defun zz/vterm-list ()
  (let (zz/vterms)
    (dolist (b (buffer-list))
      (let ((name (buffer-name b))
            (base multi-vterm-buffer-name))
        (when (string-match (format "^\\*%s<[0-9]+>\\*$" base) name)
          (push name zz/vterms))))
    (nreverse zz/vterms)))

(defun zz/switch-to-vterm (buf-name)
  (interactive
   (list (ido-completing-read "Vterm name: " (zz/vterm-list))))
  (if (and buf-name (not (string-empty-p buf-name)) (get-buffer buf-name))
      (progn
        (switch-to-buffer buf-name)
        (message "Switched to %s" buf-name)
        (delete-other-windows))
      (multi-vterm)
      (delete-other-windows)
      (message "Created and switched to new: %s" (buffer-name (current-buffer)))))

(defun zz/get-remote-vterm (host)
  "Connect to a remote host."
  (let ((multi-vterm-program "ssh")
        (multi-vterm-program-switches host))
    (multi-vterm))
  (message "Remote %s ready" host))

(defun zz/remote-vterm ()
  "Connect to a remote term by parsing ssh config and using vterm."
  (interactive)
  (with-temp-buffer
    (let* ((path (mapconcat
                  (lambda (x)
                    (when (file-exists-p x)
                      (concat x)))
                  '("~/.ssh/config"
                    "~/.ssh/config.d/*")
                  " "))
           (grep "grep -i -e '^host ' | grep -v '[*?]' | grep -v 'git'")
           (awk "awk '/^Host/{if (NR!=1)print \"\"; printf $2}'")
           (command (format "cat %s | %s | %s" path grep awk))
           (host (ido-completing-read "Host: "
                                      (split-string
                                       (shell-command-to-string command)))))
      (zz/get-remote-vterm host))))


(provide 'vterm-custom)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-custom.el ends here
