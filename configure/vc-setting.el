;;;version control setting
;;

(zz:load-path "elisp")

;;add mode support
(setq auto-mode-alist
   (append
    (list (cons "\\.hgrc$"         'conf-mode))
    (list (cons "\\.gitconfig$"    'conf-mode))
    auto-mode-alist))

;;mercurial support
(require 'mercurial)
(require 'mq)

(zz:load-path "site-lisp/monky")
(require 'monky)
(setq monky-process-type 'cmdserver)

;;magit
;;M-x magit-log-buffer-file
;;M-x magit-find-file
;;File log: C-x v l
(zz:load-path "site-lisp/magit/lisp")
(require 'magit)
(defkeys-map global-map
  ((kbd "M-g M-s") 'magit-status)
  ((kbd "M-g M-b") 'magit-blame)
  ((kbd "M-g M-d") 'magit-dispatch))
(setq magit-completing-read-function 'helm--completing-read-default)
(setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

;;magit-lfs
(require 'magit-lfs)

;;git show
(require 'git-show)

;;diffstat
(require 'diffstat)
(add-hook 'diff-mode-hook (lambda ()
                            (local-set-key "\C-c\C-l" 'diffstat)))

;;github pull request
(require 'github-pullrequest)

;;gitlab ci mode
(require 'gitlab-ci-mode)

;;git-gutter
;;https://github.com/emacsorphanage/git-gutter
;;https://github.com/nonsequitur/git-gutter-plus
(require 'git-gutter+)

;;https://github.com/emacsmirror/git-timemachine
(require 'git-timemachine)
(defkeys-map global-map
  ((kbd "M-g M-t") 'git-timemachine-toggle))

;;git elisp interface
(require 'git)


(provide 'vc-setting)

;;; vc-setting.el ends here
