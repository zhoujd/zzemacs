;;;;go programe setting
;;;

(zz-load-path "site-lisp/go-mode")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(provide 'go-setting)

;;;; go-setting.el end here
