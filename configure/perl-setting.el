;;;perl programme setting

(zz-load-path "site-lisp/pde/lisp")
(load "pde-load")

(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-h f") 'cperl-perldoc)))

(provide 'perl-setting)

;; perl-setting.el end here
