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
      (when (string-match "^\\*vterminal<[0-9]+>\\*$" (buffer-name b))
        (push (buffer-name b) zz/vterms)))
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
  "Connect to a remote host using standard vterm."
  (let* ((buffer-name (format "*vterm ssh: %s*" host))
         (vterm-buffer (get-buffer buffer-name)))
    (if vterm-buffer
        (pop-to-buffer vterm-buffer)
        (with-current-buffer (generate-new-buffer buffer-name)
          (vterm-mode)
          (setq-local vterm-shell (executable-find "ssh"))
          (setq-local vterm-kill-buffer-on-exit t)
          (setq-local vterm-buffer-name buffer-name)
          (vterm-send-string (format "ssh %s\n" host))
          (pop-to-buffer (current-buffer))))
    (message "Remote %s ready via vterm" host)))

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
