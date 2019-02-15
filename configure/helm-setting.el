;;;; helm-setting.el --- helm setting file
;;;

(zz-load-path "site-lisp/emacs-async")
(zz-load-path "site-lisp/helm")
(require 'helm)
(require 'helm-config)

(helm-mode t)

;;no completion
(mapc
 (lambda (mode)
   (add-to-list 'helm-mode-no-completion-in-region-in-modes mode))
 '(shell-mode
   gdb-mode
   ))

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

(zz-load-path "elisp")
(require 'helm-switchb)

(require 'helm-find)
(defun zz:helm-find ()
  (interactive)
  (helm-find-1 (file-name-as-directory
                (ido-read-directory-name "Directory: "))))

(require 'helm-grep)
(defun zz:helm-grep-ag ()
  (interactive)
  (helm-grep-ag-1 (file-name-as-directory
                   (ido-read-directory-name "Directory: "))))


(provide 'helm-setting)

;;; helm-setting.el ends here
