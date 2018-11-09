;;;; helm-setting.el --- helm setting file
;;;

(zz-load-path "elisp")
(require 'apply-keys)

(zz-load-path "site-lisp/emacs-async")
(zz-load-path "site-lisp/helm")
(require 'helm)
(require 'helm-config)

(helm-mode t)
(helm-autoresize-mode t)

;;no completion for shell-mode
(add-to-list 'helm-mode-no-completion-in-region-in-modes 'shell-mode)

;;https://writequit.org/eos/eos-helm.html
(setq helm-display-header-line nil
      helm-always-two-windows nil
      helm-split-window-in-side-p t
      helm-candidate-number-limit 100
      helm-input-idle-delay 0
      helm-tramp-verbose 0
      helm-move-to-line-cycle-in-source t)

(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found))
(setq helm-apropos-function-list '(helm-def-source--emacs-commands
                                   helm-def-source--emacs-functions
                                   helm-def-source--emacs-variables
                                   helm-def-source--emacs-faces))
;; List of times to show in helm-world-time
(setq display-time-world-list '(("Asia/Shanghai" "Shanghai")
                                ("PST8PDT" "Mountain View")
                                ("Europe/London" "London")
                                ("Australia/Sydney" "Sydney")))

(setq helm-autoresize-min-height 20)

(require 'helm-smex)
(require 'helm-etags-plus)

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x b")      'helm-buffers-list
  (kbd "C-M-z")      'helm-resume
  (kbd "C-h M-a")    'helm-apropos
  (kbd "C-h M-t")    'helm-world-time
  (kbd "C-h M-g")    'helm-grep-do-git-grep
  (kbd "C-h M-m")    'helm-man-woman

  (kbd "M-.")        'helm-etags-plus-select
  (kbd "M-*")        'helm-etags-plus-history
  (kbd "M-_")        'helm-etags-plus-history-go-back
  (kbd "M-+")        'helm-etags-plus-history-go-forward
  ))

(apply-keys-to-map
 helm-map
 (list
  (kbd "C-<return>") 'helm-execute-persistent-action
  (kbd "C-p")        'helm-previous-line
  (kbd "C-n")        'helm-next-line
  (kbd "C-M-n")      'helm-next-source
  (kbd "C-M-p")      'helm-previous-source
  (kbd "M-N")        'helm-next-source
  (kbd "M-P")        'helm-previous-source
  ))


(provide 'helm-setting)

;;; helm-setting.el ends here
