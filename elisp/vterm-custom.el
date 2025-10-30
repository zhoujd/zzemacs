;;;; vterm-custom.el --- sample config file
;;;


(zz/load-path "site-lisp/emacs-vterm")

(require 'vterm)
(require 'multi-vterm)
(require 'vterm-toggle)

(defun zz/vterm-hook ()
  (defkeys-map vterm-mode-map
    ((kbd "C-q") 'vterm-send-next-key)
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


(provide 'vterm-custom)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vterm-custom.el ends here
