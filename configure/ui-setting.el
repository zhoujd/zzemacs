;;;; ui-setting.el --- ui config file

;;mode-line
(zz:load-path "site-lisp/mood-line")
(require 'mood-line)
(setq mood-line-glyph-alist mood-line-glyphs-fira-code)
(mood-line-mode t)

;;modern-fringes
(require 'modern-fringes)
(modern-fringes-invert-arrows)


(provide 'ui-setting)

;;;; ui-setting.el --- end here
