;;;groovy program setting
;;On Ubuntu
;;apt search openjdk
;;apt install openjdk-16-jdk*
;;apt install openjdk-16-jre*
;;https://groovy.apache.org/download.html

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
