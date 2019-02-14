;;;; helm-switchb.el --- helm switch buffer
;;;

(require 'helm)

(defmacro helm-switchb-candidate (mode)
  `(lambda ()
     (mapcar
      (lambda (buf)
        (format "%-30s %s"
                (buffer-name buf)
                (with-current-buffer (buffer-name buf)
                  default-directory)))
      (cl-remove-if-not
       (lambda (buf)
         (with-current-buffer buf
           (eq major-mode ,mode)))
       (buffer-list)))))

(defun helm-switchb-select (candidate)
     (switch-to-buffer (car (split-string candidate))))

(defvar helm-switchb-shell-source
  (helm-build-sync-source "Shell buffers"
    :candidates (helm-switchb-candidate 'shell-mode)
    :action '(("Switch to buffer" . helm-switchb-select))
    ))

(defvar helm-switchb-term-source
  (helm-build-sync-source "Multi-term buffers"
    :candidates (helm-switchb-candidate 'term-mode)
    :action '(("Switch to buffer" . helm-switchb-select))
    ))

(defun helm-switchb-shell-list ()
  (interactive)
  (helm :sources '(helm-switchb-shell-source
                   helm-switchb-term-source)
        :buffer "*helm shell*"
        :truncate-lines helm-buffers-truncate-lines
        ))

(defvar helm-switchb-dired-source
  (helm-make-source "Dired Buffers" 'helm-source-buffers
    :buffer-list
    (lambda ()
      (mapcar #'buffer-name
              (cl-remove-if-not
               (lambda (buf)
                 (with-current-buffer buf
                   (eq major-mode 'dired-mode)))
               (buffer-list))))))

(defun helm-switchb-dired-list ()
  (interactive)
  (helm :sources helm-switchb-dired-source
        :buffer "*helm dired*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))


(provide 'helm-switchb)

;;; helm-switchb.el ends here
