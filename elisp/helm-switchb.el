;;;; helm-switchb.el --- helm switch buffer
;;;

(require 'helm)

(defvar helm-source-shell-list
  (helm-build-sync-source "Shell buffers"
    :candidates (lambda ()
                  (mapcar 
                   (lambda (buf)
                     (format "%-20s %s"
                             (buffer-name buf)
                             (with-current-buffer (buffer-name buf)
                               default-directory)))
                   (cl-remove-if-not
                    (lambda (buf)
                      (with-current-buffer buf
                        (eq major-mode 'shell-mode)))
                    (buffer-list))))
    :action '(("Switch to buffer" . (lambda (select)
                                      (switch-to-buffer (car (split-string select))))))))

(defvar helm-source-term-list
  (helm-build-sync-source "Multi-term buffers"
    :candidates (lambda ()
                  (mapcar (lambda (buf)
                            (format "%-20s %s"
                                    (buffer-name buf)
                                    (with-current-buffer (buffer-name buf)
                                      default-directory)))
                          (cl-remove-if-not
                           (lambda (buf)
                             (with-current-buffer buf
                               (eq major-mode 'term-mode)))
                           (buffer-list))))
    :action '(("Switch to buffer" . (lambda (select)
                                      (switch-to-buffer (car (split-string select))))))))

(defun helm-shell-buffers-list ()
  (interactive)
  (helm :sources '(helm-source-shell-list
                   helm-source-term-list)
        :buffer "*helm shell*"
        :truncate-lines helm-buffers-truncate-lines
        ))

(defvar helm-source-dired-buffers-list
  (helm-make-source "Dired Buffers" 'helm-source-buffers
    :buffer-list
    (lambda ()
      (mapcar #'buffer-name
              (cl-remove-if-not
               (lambda (buf)
                 (with-current-buffer buf
                   (eq major-mode 'dired-mode)))
               (buffer-list))))))

(defun helm-dired-buffers-list ()
  (interactive)
  (helm :sources helm-source-dired-buffers-list
        :buffer "*helm dired*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))


(provide 'helm-switchb)

;;; helm-switchb.el ends here
