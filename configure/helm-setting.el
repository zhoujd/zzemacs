;;;; helm-setting.el --- helm setting file
;; https://github.com/emacs-helm/helm
;; https://github.com/emacs-helm/helm/wiki

(zz:load-path "site-lisp/emacs-async")
(zz:load-path "site-lisp/helm")
(require 'helm)
(require 'helm-config)

;;https://github.com/emacs-helm/helm/blob/master/helm.el#L1432
;;| M-o     | left   | Previous source                                   |
;;| C-o     | right  | Next source                                       |
;;| C-j     |        | Persistent action (Execute and keep Helm session) |
;;| C-c ?   |        | helm help                                         |
;;| C-c r   |        | sudo open                                         |
;;| C-]     |        | toggle between showing filenames or full pathnames|
(helm-mode t)

;;no completion
(mapc (lambda (mode)
        (add-to-list 'helm-mode-no-completion-in-region-in-modes mode))
      '(shell-mode))

(setq helm-autoresize-min-height 20)
(helm-autoresize-mode t)

;;helm-tramp
(require 'helm-tramp)

;;https://writequit.org/eos/eos-helm.html
(setq helm-display-header-line          nil
      helm-echo-input-in-header-line    nil
      helm-always-two-windows           nil
      helm-split-window-in-side-p       t
      helm-candidate-number-limit       100
      helm-input-idle-delay             0
      helm-tramp-verbose                0
      helm-tramp-default-method         "sshz"
      helm-move-to-line-cycle-in-source t
      helm-ff-preferred-shell-mode      'shell-mode
      helm-mini-default-sources         '(helm-source-buffers-list
                                          helm-source-recentf
                                          helm-source-bookmarks
                                          helm-source-buffer-not-found)
      helm-apropos-function-list        '(helm-def-source--emacs-commands
                                          helm-def-source--emacs-functions
                                          helm-def-source--emacs-variables
                                          helm-def-source--emacs-faces))

;;https://github.com/ptrv/helm-smex
(require 'helm-smex)
(require 'helm-etags-plus)

;;https://github.com/emacs-helm/helm-descbinds
;;C-h b
(require 'helm-descbinds)
(defun zz:ctrl-z-help ()
  (interactive)
  (helm-descbinds (kbd "C-z h")))

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

(zz:load-path "elisp")
(require 'helm-switchb)

(require 'helm-find)
(defun zz:helm-find ()
  (interactive)
  (let ((default-directory (file-name-as-directory
                            (ido-read-directory-name "Directory: "))))
    (helm-find-1 default-directory)))

;;See file content temporarily by persistent action(C-j).
;;For ack: helm-ag-base-command "ack --nocolor --nogroup"
;;https://github.com/emacsorphanage/helm-ag
;;https://github.com/ggreer/the_silver_searcher
;;apt/yum/dnf install silversearcher-ag
;;pacman -S the_silver_searcher
;;yum install epel-release.noarch the_silver_searcher
(zz:load-path "site-lisp/popup")
(require 'popup)
(require 'helm-ag)

;;https://github.com/ggreer/the_silver_searcher
;;apt/dnf/yum install silversearcher-ag
(defun zz:helm-ag-switch-to-ag-inexact()
  "case-insensitive and without word boundaries."
  (interactive)
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
        helm-ag-command-option "--all-text"
        helm-ag-insert-at-point 'symbol)
  (message "helm-ag switch to ag"))

(defun zz:helm-ag-switch-to-ag-exact()
  "case-sensitive and with word boundaries."
  (interactive)
  (setq helm-ag-base-command "ag --nocolor --nogroup --word-regexp --case-sensitive"
        helm-ag-command-option "--all-text"
        helm-ag-insert-at-point 'symbol)
  (message "helm-ag switch to ag exact"))

;;https://beyondgrep.com/install/
;;https://beyondgrep.com/more-tools/
;;Ubuntu: sudo apt install ack-grep
(defun zz:helm-ag-switch-to-ack()
  (interactive)
  (setq helm-ag-command-option ""
        helm-ag-base-command "ack --nocolor --nogroup"
        helm-ag-insert-at-point 'symbol)
  (message "helm-ag switch to ack"))

;;https://github.com/eliben/pss/
;;pip install pss
(defun zz:helm-ag-switch-to-pss()
  (interactive)
  (setq helm-ag-command-option ""
        helm-ag-base-command "pss --nocolor --noheading"
        helm-ag-insert-at-point 'symbol)
  (message "helm-ag switch to pss"))

;;helm default using ag
(zz:helm-ag-switch-to-ag-inexact)

(require 'helm-grep)
(custom-set-variables
 '(helm-grep-file-path-style 'relative))

(defun zz:helm-grep-ag ()
  (interactive)
  (let ((default-directory (file-name-as-directory
                            (ido-read-directory-name "Directory: "))))
    (helm-do-ag default-directory)))

(require 'helm-cscope)
(require 'helm-bm)

(require 'helm-shell)
(add-hook 'comint-mode-hook
          (lambda ()
            (defkeys-map comint-mode-map
              ((kbd "M-s f")   'helm-comint-prompts-all)
              ((kbd "M-s M-f") 'helm-comint-prompts)
              )))
;;helm proc
(require 'helm-proc)

;;helm-mt
(require 'helm-mt)

;;helm-ispell
(require 'helm-ispell)

;;helm-sudo-find-file
(require 'sudo-edit)
(defalias 'helm-sudo-find-file 'sudo-edit-find-file)

;;helm-google
(require 'helm-google)

;;helm-ssh-tunnel
(zz:load-path "site-lisp/ssh-tunnels")
(require 'ssh-tunnels)
(require 'helm-ssh-tunnels)

;;helm-switch-to-repl
(require 'helm-switch-to-repl)
(helm-switch-to-repl-setup)


(provide 'helm-setting)

;;; helm-setting.el ends here
