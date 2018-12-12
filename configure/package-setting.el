;;;; package-setting.el
;;https://melpa.org

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;;emacs proxy
;(setq url-proxy-services
;   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;     ("http" . "proxy.com:8080")
;     ("https" . "proxy.com:8080")))


(provide 'package-setting)

;;; package-setting.el ends here
