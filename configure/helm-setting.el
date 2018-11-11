;;;; helm-setting.el --- helm setting file
;;;

(zz-load-path "site-lisp/emacs-async")
(zz-load-path "site-lisp/helm")
(require 'helm)
(require 'helm-config)

(helm-mode t)

(setq helm-autoresize-min-height 20)
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

;;https://github.com/ptrv/helm-smex
(require 'helm-smex)
(require 'helm-etags-plus)

(require 'helm-descbinds)
(helm-descbinds-mode)


(provide 'helm-setting)

;;; helm-setting.el ends here
