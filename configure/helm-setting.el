;;;; helm-setting.el --- helm setting file
;;https://github.com/emacs-helm/helm
;;https://github.com/emacs-helm/helm/wiki

(zz:load-path "site-lisp/async")
(zz:load-path "site-lisp/helm")
(require 'helm)
(require 'helm-config)

;;https://github.com/emacs-helm/helm/blob/master/helm-core.el#L392
;;helm/helm-core.el:L392
;;| M-o     | left   | Previous source                                     |
;;| C-o     | right  | Next source                                         |
;;| C-j     |        | Persistent action (Execute and keep Helm session)   |
;;| C-c ?   |        | helm help                                           |
;;| C-c r   |        | sudo open                                           |
;;| C-]     |        | Toggle between showing filenames or full pathnames  |
;;| C-u     | C-M-Z  | Choose between different Helm sessions (helm-resume)|
;;| C-t     |        | Toggle resplit and swap helm windows                |
(helm-mode t)

;;no completion
(mapc (lambda (mode)
        (add-to-list 'helm-mode-no-completion-in-region-in-modes mode))
      '(shell-mode))

;;auto resize
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode t)

;;helm-tramp
(require 'helm-tramp)
(setq helm-tramp-docker-user "jenkins")

;;https://writequit.org/eos/eos-helm.html
(setq helm-display-header-line          nil
      helm-echo-input-in-header-line    nil
      helm-always-two-windows           nil
      helm-split-window-in-side-p       t
      helm-candidate-number-limit       9999
      helm-input-idle-delay             0
      helm-tramp-verbose                0
      helm-move-to-line-cycle-in-source t
      helm-lisp-fuzzy-completion        t
      helm-semantic-fuzzy-match         t
      helm-imenu-fuzzy-match            t
      helm-buffer-max-length            20
      helm-ff-preferred-shell-mode      'shell-mode
      helm-mini-default-sources         '(helm-source-buffers-list
                                          helm-source-recentf
                                          helm-source-bookmarks
                                          helm-source-buffer-not-found)
      helm-apropos-function-list        '(helm-def-source--emacs-commands
                                          helm-def-source--emacs-functions
                                          helm-def-source--emacs-variables
                                          helm-def-source--emacs-faces))

;;dide some buffers in helm-buffers-list
(setq helm-boring-buffer-regexp-list
      (append
       (list (rx "*tramp"))
       helm-boring-buffer-regexp-list))

;;https://github.com/ptrv/helm-smex
;;(require 'helm-smex)

;;https://github.com/jixiuf/helm-etags-plus
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

;;https://github.com/cosmicexplorer/helm-rg
;;https://github.com/BurntSushi/ripgrep/releases
(require 'helm-rg)

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
;;apt/dnf/yum install ripgrep
;;helm-ag-base-command "grep -rin"
;;helm-ag-base-command "csearch -n"
;;helm-ag-base-command "pt --nocolor --nogroup"
;;helm-ag-base-command "rg --vimgrep --no-heading"
;;ag -all-text (-t) search it'll override --ignore
(defun zz:helm-ag-switch-to-ag-inexact()
  "case-insensitive and without word boundaries."
  (interactive)
  (custom-set-variables
   '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
   '(helm-ag-command-option "--silent --skip-vcs-ignores --ignore=TAGS --ignore=cscope.*")
   '(helm-ag-insert-at-point 'symbol))
  (message "helm-ag switch to ag"))

(defun zz:helm-ag-switch-to-ag-exact()
  "case-sensitive and with word boundaries."
  (interactive)
  (custom-set-variables
   '(helm-ag-base-command "ag --nocolor --nogroup --word-regexp --case-sensitive")
   '(helm-ag-command-option "--silent --skip-vcs-ignores --ignore=TAGS --ignore=cscope.*")
   '(helm-ag-insert-at-point 'symbol))
  (message "helm-ag switch to ag exact"))

;;https://beyondgrep.com/install/
;;https://beyondgrep.com/more-tools/
;;Ubuntu: sudo apt install ack-grep
(defun zz:helm-ag-switch-to-ack()
  (interactive)
  (custom-set-variables
   '(helm-ag-base-command "ack --nocolor --nogroup")
   '(helm-ag-command-option "")
   '(helm-ag-insert-at-point 'symbol))
  (message "helm-ag switch to ack"))

;;https://github.com/eliben/pss/
;;pip install pss
(defun zz:helm-ag-switch-to-pss()
  (interactive)
  (custom-set-variables
   '(helm-ag-base-command "pss --nocolor --noheading")
   '(helm-ag-command-option "")
   '(helm-ag-insert-at-point 'symbol))
  (message "helm-ag switch to pss"))

(defun zz:helm-ag-switch-to-rg()
  (interactive)
  (custom-set-variables
   '(helm-ag-base-command "rg --vimgrep --no-heading --smart-case")
   '(helm-ag-command-option "")
   '(helm-ag-insert-at-point 'symbol))
  (message "helm-ag switch to rg"))

(defun zz:helm-ag-switch-to-grep-inexact()
  (interactive)
  (custom-set-variables
   '(helm-ag-base-command "grep -rinI")
   '(helm-ag-command-option "")
   '(helm-ag-insert-at-point 'symbol))
  (message "helm-ag switch to grep inexact"))

(defun zz:helm-ag-switch-to-grep-exact()
  (interactive)
  (custom-set-variables
   '(helm-ag-base-command "grep -rnI")
   '(helm-ag-command-option "")
   '(helm-ag-insert-at-point 'symbol))
  (message "helm-ag switch to grep exact"))

;;helm default using ag
(defun zz:helm-search-select()
  (cond ((executable-find "ag") (zz:helm-ag-switch-to-ag-inexact))
        ((executable-find "ack") (zz:helm-ag-switch-to-ack))
        (t (zz:helm-ag-switch-to-grep-inexact))))
(zz:helm-search-select)

(require 'helm-grep)
(custom-set-variables
 '(helm-grep-file-path-style 'relative))

(defun zz:helm-grep-ag ()
  (interactive)
  (let ((default-directory (file-name-as-directory
                            (ido-read-directory-name "Directory: "))))
    (helm-do-ag default-directory)))

(defun zz:nnn ()
  (interactive)
  (let* ((default-directory (file-name-as-directory
                            (ido-read-directory-name "Directory: ")))
         (cmd (list "urxvt" "-name" "nnn" "-e" "nnn" "-e" default-directory)))
    (apply 'start-process "urxvt" nil cmd)))

(defun zz:helm-nnn (dir)
  (interactive "DDirectory: ")
  (let* ((default-directory dir)
         (cmd (list "urxvt" "-name" "nnn" "-e" "nnn" "-e" default-directory)))
    (apply 'start-process "urxvt" nil cmd)))

(defun zz:helm-compile (dir)
  (interactive "DDirectory: ")
  (let* ((default-directory dir))
    (call-interactively 'compile)))

;;https://github.com/alpha22jp/helm-cscope.el
;;C-c s I     Create list and index
(require 'helm-cscope)
(add-hook 'c-mode-common-hook 'helm-cscope-mode)
(add-hook 'helm-cscope-mode-hook
          (lambda ()
            (local-set-key (kbd "M-s M-.") 'helm-cscope-find-global-definition)
            (local-set-key (kbd "M-s M-d") 'helm-cscope-find-global-definition)
            (local-set-key (kbd "M-s M-c") 'helm-cscope-find-calling-this-funtcion)
            (local-set-key (kbd "M-s M-@") 'helm-cscope-find-calling-this-function)
            (local-set-key (kbd "M-s M-C") 'helm-cscope-find-called-function)
            (local-set-key (kbd "M-s M-s") 'helm-cscope-find-this-symbol)
            (local-set-key (kbd "M-s M-f") 'helm-cscope-find-this-file)
            (local-set-key (kbd "M-s M-t") 'helm-cscope-find-this-text-string)
            (local-set-key (kbd "M-s M-e") 'helm-cscope-find-egrep-pattern)
            (local-set-key (kbd "M-s M-i") 'helm-cscope-find-files-including-file)
            (local-set-key (kbd "M-s M-=") 'helm-cscope-find-assignments-to-this-symbol)
            (local-set-key (kbd "M-s M-,") 'helm-cscope-pop-mark)
            (local-set-key (kbd "M-s M-p") 'helm-cscope-pop-mark)))

(require 'helm-bm)

(require 'helm-shell)
(add-hook 'comint-mode-hook
          (lambda ()
            (defkeys-map comint-mode-map
              ((kbd "M-s f")   'helm-comint-prompts)
              ((kbd "M-s M-f") 'helm-comint-prompts-all)
              )))

;;helm proc
(require 'helm-proc)

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

;;helm-lsp
;;https://github.com/emacs-lsp/helm-lsp
(require 'helm-lsp)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

;;helm-mode-manager
;;M-x: helm-switch-major-mode
;;M-x: helm-enable-minor-mode
;;M-x: helm-disable-minor-mode
(require 'helm-mode-manager)

;;helm-themes
(require 'helm-themes)

;;helm-gitignore
(zz:load-path "site-lisp/git-modes")
(require 'helm-gitignore)

;;helm-systemd
(require 'helm-systemd)

;;helm-gtags
;(require 'helm-gtags)

;;helm-cmd-t
(zz:load-path "site-lisp/helm-cmd-t")
(require 'helm-for-files)
(require 'helm-x-files)
(require 'helm-cmd-t)
(require 'helm-C-x-b)

;;helm-lsp
(require 'helm-lsp)
(defkeys-map lsp-mode-map
  ([remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;;https://github.com/Sodel-the-Vociferous/helm-company
;;"Making tag completion table" Freezes/Blocks
;;How to disable
(require 'helm-company)
(setq helm-company-show-icons nil)
(defun helm-company-setup ()
  (local-set-key (kbd "C-<tab>") 'helm-company))
(mapc (lambda (hook)
        (add-hook hook 'helm-company-setup))
      '(emacs-lisp-mode-hook
        python-mode-hook
        shell-mode-hook
        c-mode-hook
        c++-mode-hook))

;;helm-flx
(require 'helm-flx)
(helm-flx-mode +1)

;;wgrep-helm
(zz:load-path "site-lisp/wgrep")
(require 'wgrep-helm)

;;helm-describe-modes
(require 'helm-describe-modes)


(provide 'helm-setting)

;;; helm-setting.el ends here
