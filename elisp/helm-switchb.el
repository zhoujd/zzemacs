;;;; helm-switchb.el --- helm switch buffer
;;;

(require 'helm)
(require 'multi-shell)

(defvar helm-switchb-separator " "
  "helm switchb separator")

(defmacro helm-switchb-candidate (mode)
  "Collect and format candidates base on Major mode"
  `(lambda ()
     (mapcar
      (lambda (buf)
        (format "%-27s%s%s"
                (buffer-name buf)
                helm-switchb-separator
                (with-current-buffer (buffer-name buf)
                  default-directory)))
      (cl-remove-if-not
       (lambda (buf)
         (with-current-buffer buf
           (eq major-mode ,mode)))
       (buffer-list)))))

(defmacro helm-switchb-run (&rest body)
  "Define an action with BODY to be run after exiting Helm"
  (declare (doc-string 1))
  `(lambda ()
     (interactive)
     (with-helm-alive-p
       (helm-exit-and-execute-action (lambda (_candidate) ,@body)))))

(defun helm-switchb-select (candidate)
  (switch-to-buffer (car (split-string candidate helm-switchb-separator))))

(defun helm-switchb-dired-open (candidate)
  (dired (car (reverse (split-string candidate helm-switchb-separator)))))

(defun helm-switchb-kill (candidate)
  (loop for cand in (helm-marked-candidates)
        do
        (kill-buffer (car (split-string cand helm-switchb-separator)))))

(defun helm-switchb-shell-new (candidate)
  (let ((default-directory (car (reverse (split-string candidate helm-switchb-separator)))))
    (multi-shell-new)))

(defun helm-switchb-term-new (candidate)
  (let ((default-directory (car (reverse (split-string candidate helm-switchb-separator)))))
    (multi-term)))

(defun helm-switcb-kill-shell ()
  "kill shell buffer"
  (interactive)
  (helm-switchb-kill (helm-marked-candidates)))

(defun helm-switcb-open-dired ()
  "open dired buffer"
  (interactive)
  (helm-switchb-dired-open (helm-get-selection)))

(defvar helm-switchb-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c d")   (helm-switchb-run (helm-switcb-kill-shell)))
    (define-key map (kbd "C-c M-d") (helm-switchb-run (helm-switcb-open-dired)))
    map)
  "Keymap for `helm-switchb'.")

(defvar helm-switchb-shell-source
  (helm-build-sync-source "Shell buffers"
    :candidates (helm-switchb-candidate 'shell-mode)
    :action '(("Switch to buffer" . helm-switchb-select)
              ("Open dired" . helm-switchb-dired-open)
              ("Kill buffer" . helm-switchb-kill)
              ("New shell" . helm-switchb-shell-new)
              ("New terminal" . helm-switchb-term-new))
    :keymap helm-switchb-map
    ))

(defvar helm-switchb-term-source
  (helm-build-sync-source "Multi-term buffers"
    :candidates (helm-switchb-candidate 'term-mode)
    :action '(("Switch to buffer" . helm-switchb-select)
              ("Open dired" . helm-switchb-dired-open)
              ("Kill buffer" . helm-switchb-kill)
              ("New shell" . helm-switchb-shell-new)
              ("New terminal" . helm-switchb-term-new))
    :keymap helm-switchb-map
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
  (helm :sources '(helm-switchb-dired-source)
        :buffer "*helm dired*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))


(provide 'helm-switchb)

;;; helm-switchb.el ends here
