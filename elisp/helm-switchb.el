;;;; helm-switchb.el --- helm switch buffer
;;;

(require 'helm)
(require 'multi-shell)
(require 'multi-term)

(zz:load-path "site-lisp/emacs-libvterm")
(require 'multi-vterm)

(defvar helm-switchb-separator " "
  "helm switchb separator")

(defvar helm-switchb-ignores '("*Async Shell Command*")
  "helm switchb ignores buffers")

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
      (progn
        (cl-remove-if
         (lambda (buf)
           (or (member (buffer-name buf) helm-switchb-ignores)
               (with-current-buffer buf
                 (neq major-mode ,mode))))
         (buffer-list))))))

(defmacro helm-switchb-run (&rest body)
  "Define an action with BODY to be run after exiting Helm"
  (declare (doc-string 1))
  `(lambda ()
     (interactive)
     (with-helm-alive-p
       (helm-exit-and-execute-action
        (lambda (_candidate) ,@body)))))

(defun helm-switchb-select (candidate)
  (switch-to-buffer
   (car (split-string
         candidate helm-switchb-separator t))))

(defun helm-switchb-dired-open (candidate)
  (dired (car (reverse
               (split-string
                candidate helm-switchb-separator t)))))

(defun helm-switchb-kill (candidate)
  (loop for cand in (helm-marked-candidates)
        do
        (kill-buffer (car (split-string
                           cand helm-switchb-separator t)))))

(defun helm-switchb-shell-new (candidate)
  (let ((default-directory
          (car (reverse (split-string
                         candidate helm-switchb-separator t)))))
    (multi-shell-new)))

(defun helm-switchb-term-new (candidate)
  (let ((default-directory
          (car (reverse (split-string
                         candidate helm-switchb-separator t)))))
    (multi-term)))

(defun helm-switchb-vterm-new (candidate)
  (let ((default-directory
          (car (reverse (split-string
                         candidate helm-switchb-separator t)))))
    (multi-vterm)))

(defun helm-switchb-kill-shell ()
  "kill shell buffer"
  (interactive)
  (helm-switchb-kill (helm-marked-candidates)))

(defun helm-switchb-open-dired ()
  "open dired buffer"
  (interactive)
  (helm-switchb-dired-open (helm-get-selection)))

(defun helm-switchb-open-shell ()
  "open shell buffer"
  (interactive)
  (helm-switchb-shell-new (helm-get-selection)))

(defun helm-switchb-open-term ()
  "open term buffer"
  (interactive)
  (helm-switchb-term-new (helm-get-selection)))

(defun helm-switchb-open-vterm ()
  "open term buffer"
  (interactive)
  (helm-switchb-vterm-new (helm-get-selection)))

(defvar helm-switchb-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c d")   (helm-switchb-run (helm-switchb-kill-shell)))
    (define-key map (kbd "C-c M-d") (helm-switchb-run (helm-switchb-open-dired)))
    (define-key map (kbd "C-c s")   (helm-switchb-run (helm-switchb-open-shell)))
    (define-key map (kbd "C-c t")   (helm-switchb-run (helm-switchb-open-term)))
    (define-key map (kbd "C-c v")   (helm-switchb-run (helm-switchb-open-vterm)))
    map)
  "Keymap for `helm-switchb'.")

(defvar helm-switchb-shell-source
  (helm-build-sync-source "Shell buffers"
    :candidates (helm-switchb-candidate 'shell-mode)
    :action '(("Switch to buffer" . helm-switchb-select)
              ("Open dired" . helm-switchb-dired-open)
              ("Kill buffer" . helm-switchb-kill)
              ("New shell" . helm-switchb-shell-new)
              ("New vterm" . helm-switchb-vterm-new)
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
              ("New vterm" . helm-switchb-vterm-new)
              ("New terminal" . helm-switchb-term-new))
    :keymap helm-switchb-map
    ))

(defvar helm-switchb-vterm-source
  (helm-build-sync-source "Multi-vterm buffers"
    :candidates (helm-switchb-candidate 'vterm-mode)
    :action '(("Switch to buffer" . helm-switchb-select)
              ("Open dired" . helm-switchb-dired-open)
              ("Kill buffer" . helm-switchb-kill)
              ("New shell" . helm-switchb-shell-new)
              ("New vterm" . helm-switchb-vterm-new)
              ("New terminal" . helm-switchb-term-new))
    :keymap helm-switchb-map
    ))

(defun helm-switchb-shell-list ()
  (interactive)
  (helm :sources '(helm-switchb-shell-source
                   helm-switchb-term-source
                   helm-switchb-vterm-source)
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
