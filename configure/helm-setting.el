;;;; helm-setting.el --- helm setting file
;;;

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
      helm-split-window-in-side-p t
      helm-candidate-number-limit 100
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

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-h M-a") 'helm-apropos)
(global-set-key (kbd "C-h M-t") 'helm-world-time)

(define-key helm-map (kbd "C-<return>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-p")   'helm-previous-line)
(define-key helm-map (kbd "C-n")   'helm-next-line)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(define-key helm-map (kbd "M-N")   'helm-next-source)
(define-key helm-map (kbd "M-P")   'helm-previous-source)

(require 'helm-etags-plus)
(global-set-key "\M-." 'helm-etags-plus-select)
(global-set-key "\M-*" 'helm-etags-plus-history)
(global-set-key "\M-_" 'helm-etags-plus-history-go-back)
(global-set-key "\M-+" 'helm-etags-plus-history-go-forward)


(provide 'helm-setting)

;;; helm-setting.el ends here
