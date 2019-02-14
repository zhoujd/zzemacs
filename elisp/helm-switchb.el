;;;; helm-switchb.el --- helm switch buffer
;;;

(require 'helm)
(require 'multi-term)
(require 'multi-shell)

(defvar helm-switchb-shell-source
  (helm-build-sync-source "Shell buffers"
    :candidates (lambda ()
                  (mapcar 
                   (lambda (buf)
                     (format "%-30s %s"
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

(defvar helm-switchb-term-source
  (helm-build-sync-source "Multi-term buffers"
    :candidates (lambda ()
                  (mapcar (lambda (buf)
                            (format "%-30s %s"
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
