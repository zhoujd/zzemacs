;;;version control setting
;;

(zz:load-path "elisp")

;;add mode support
(setq auto-mode-alist
   (append
    (list (cons "\\.hgrc$"         'conf-mode))
    (list (cons "\\.gitconfig$"    'conf-mode))
    auto-mode-alist))

;;auto check vc info
(setq auto-revert-check-vc-info t)
(setq auto-revert-interval 10)

;;svn support
;(require 'psvn)
;(require 'vc-svn)

;;git support
;;=>github ssh-keygen
;;$ssh-keygen -t rsa -C "your_email@example.com"
;;;=>gitisos ssh-keygen
;;$ssh-keygen -t dsa
;;$ssh -T git@github.com
;;;=>SSH authorized each other
;;$ssh-keygen -t rsa
;;$ssh-keygen -p -f ~/.ssh/id_rsa
;;$touch ~/.ssh/authorized_keys
;;$chmod 644 ~/.ssh/authorized_keys
;;cat id_rsa_zachary.pub >> ~/.ssh/authorized_keys
;;ssh-copy-id user1@123.123.123.123

;;mercurial support
(require 'mercurial)
(require 'mq)

(zz:load-path "site-lisp/monky")
(require 'monky)
(setq monky-process-type 'cmdserver)

;;git blame line
(require 'git-blame-line)

;;magit
;;M-x magit-log-buffer-file
;;M-x magit-find-file
;;File log: C-x v l
(zz:load-path "site-lisp/magit/lisp")
(require 'magit)
(defkeys-map global-map
  ((kbd "M-g M-s") 'magit-status)
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
