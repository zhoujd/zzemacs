;;;groovy program setting

;;groovy mode
(zz:load-path "site-lisp/groovy")
(require 'groovy-mode)
(require 'inf-groovy)
(setq auto-mode-alist
      (append '(("\\.groovy\\'" . groovy-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("groovy" . groovy-mode))
                                     interpreter-mode-alist))


(provide 'groovy-setting)

;;; groovy-setting.el end here
