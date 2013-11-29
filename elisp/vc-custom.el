;;;; vc-custom.el --- version control setting file
;;;

;;SVN Support
;(require 'psvn)
;(require 'vc-svn)

;;Git Support
;;github ssh-keygen
;;$ ssh-keygen -t rsa -C "your_email@example.com"
;;gitisos ssh-keygen
;;$ ssh-keygen -t dsa
;;$ ssh -T git@github.com
(zz-load-path "site-lisp/git-emacs")
(require 'git-emacs)
(require 'git-show)


(provide 'vc-custom)

;;; vc-custom.el ends here
