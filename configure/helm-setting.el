;;;; helm-setting.el --- helm setting file
;;;

(zz-load-path "site-lisp/emacs-async")
(zz-load-path "site-lisp/helm")
(require 'helm)
(require 'helm-config)

(helm-mode t)

;;no completion for shell-mode
(add-to-list 'helm-mode-no-completion-in-region-in-modes 'shell-mode)

(setq helm-autoresize-min-height 20)
(helm-autoresize-mode t)

;;https://writequit.org/eos/eos-helm.html
(setq helm-display-header-line          nil
      helm-echo-input-in-header-line    nil
      helm-always-two-windows           nil
      helm-split-window-in-side-p       t
      helm-candidate-number-limit       100
      helm-input-idle-delay             0
      helm-tramp-verbose                0
      helm-move-to-line-cycle-in-source t
      helm-mini-default-sources         '(helm-source-buffers-list
                                          helm-source-recentf
                                          helm-source-bookmarks
                                          helm-source-buffer-not-found)
      helm-apropos-function-list        '(helm-def-source--emacs-commands
                                          helm-def-source--emacs-functions
                                          helm-def-source--emacs-variables
                                          helm-def-source--emacs-faces)
      display-time-world-list           '(("Asia/Shanghai" "Shanghai")
                                          ("PST8PDT" "Mountain View")
                                          ("Europe/London" "London")
                                          ("Australia/Sydney" "Sydney")))

;;https://github.com/ptrv/helm-smex
(require 'helm-smex)
(require 'helm-etags-plus)

;;https://github.com/emacs-helm/helm-descbinds
;;C-h b
(require 'helm-descbinds)
(require 'multi-web-mode)
(helm-descbinds-mode)

;;https://github.com/ShingoFukuyama/helm-swoop
(require 'helm-swoop)
(setq helm-multi-swoop-edit-save             t
      helm-swoop-split-with-multiple-windows t
      helm-swoop-split-direction             'split-window-vertically
      helm-swoop-pre-input-function          (lambda () "")
      helm-swoop-speed-or-color              nil)

(defkeys-map isearch-mode-map
  ((kbd "M-i") 'helm-swoop-from-isearch))

(defkeys-map helm-swoop-map
  ((kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ((kbd "C-r") 'helm-previous-line)
  ((kbd "C-s") 'helm-next-line))

(defkeys-map helm-multi-swoop-map
  ((kbd "C-r") 'helm-previous-line)
  ((kbd "C-s") 'helm-next-line))

;;https://github.com/emacs-helm/helm-ls-git
(require 'helm-ls-git)

;;https://github.com/bbatsov/projectile
(require 'projectile)
(projectile-mode t)
(setq projectile-dynamic-mode-line nil)

(defkeys-map projectile-mode-map
  ((kbd "C-c p") 'projectile-command-map))

;;https://github.com/bbatsov/helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

;;helm with shell buffers
(defvar helm-source-shell-buffers-list
  (helm-make-source "Shell Buffers" 'helm-source-buffers
    :buffer-list
    (lambda ()
      (mapcar #'buffer-name
              (cl-remove-if-not
               (lambda (buf)
                 (with-current-buffer buf
                   (eq major-mode 'shell-mode)))
               (buffer-list))))))

(defun helm-shell-buffers-list ()
  (interactive)
  (helm :sources helm-source-shell-buffers-list
        :buffer "*helm shell*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))

(defvar helm-source-term-buffers-list
  (helm-make-source "Term Buffers" 'helm-source-buffers
    :buffer-list
    (lambda ()
      (mapcar #'buffer-name
              (cl-remove-if-not
               (lambda (buf)
                 (with-current-buffer buf
                   (eq major-mode 'term-mode)))
               (buffer-list))))))

(defun helm-term-buffers-list ()
  (interactive)
  (helm :sources helm-source-term-buffers-list
        :buffer "*helm term*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))

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


(provide 'helm-setting)

;;; helm-setting.el ends here
