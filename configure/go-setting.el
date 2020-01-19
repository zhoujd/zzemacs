;;;;go programe setting
;;;

;; go-mode
(zz-load-path "site-lisp/go-mode")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; company-go
(require 'company-go)


(provide 'go-setting)

;;;; go-setting.el end here
