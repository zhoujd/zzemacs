;;;; ui-setting.el --- ui config file


;;modern-fringes
(require 'modern-fringes)
(modern-fringes-invert-arrows)

;;mode-line
(zz:load-path "site-lisp/mood-line")
(require 'mood-line)
(mood-line-mode t)


(provide 'ui-setting)

;;;; ui-setting.el --- end here
