;;;vc programme setting


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
;;$touch ~/.ssh/authorized_keys
;;$chmod 644 ~/.ssh/authorized_keys
;;cat id_rsa_zachary.pub >> ~/.ssh/authorized_keys
;;ssh-copy-id user1@123.123.123.123
;;http://files.taesoo.org/git-emacs/git-emacs.html
(when (version< emacs-version "24.4")
  (zz-load-path "site-lisp/git-emacs")
  (require 'git-emacs))

;;git show
(require 'git-show)

;;mercurial Support
(require 'mercurial)
(require 'mq)

;;magit setting need upgrade for emacs24.4
(zz-load-path "site-lisp/git-modes")
(if (version< emacs-version "23.2")
    (zz-load-path "site-lisp/magit-legacy")
    (zz-load-path "site-lisp/magit"))

(require 'magit)

;;diffstat
(require 'diffstat)
(add-hook 'diff-mode-hook (lambda () (local-set-key "\C-c\C-l" 'diffstat)))

(provide 'vc-setting)

;;; vc-setting.el ends here
