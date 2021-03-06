;;;version control setting
;;

(zz:load-path "elisp")

;;add mode support
(setq auto-mode-alist
   (append
    (list (cons "\\.hgrc$"         'conf-mode))
    (list (cons "\\.gitconfig$"    'conf-mode))
    auto-mode-alist))

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
(zz:load-path "site-lisp/magit/lisp")
(require 'magit)
(defkeys-map global-map
  ((kbd "M-g M-s") 'magit-status)
  ((kbd "M-g M-c") 'magit-checkout)
  ((kbd "M-g M-b") 'git-blame-line))

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


(provide 'vc-setting)

;;; vc-setting.el ends here
