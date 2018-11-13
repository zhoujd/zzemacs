;;;; helm-setting.el --- helm setting file
;;;

(zz-load-path "site-lisp/emacs-async")
(zz-load-path "site-lisp/helm")
(require 'helm)
(require 'helm-config)

(helm-mode t)

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
(require 'helm-descbinds)
(helm-descbinds-mode)

;;https://github.com/ShingoFukuyama/helm-swoop
(require 'helm-swoop)
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)


(provide 'helm-setting)

;;; helm-setting.el ends here
