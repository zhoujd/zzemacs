;;;; ac-setting.el --- auto complete file

;; auto complete
(zz:load-path "site-lisp/auto-complete")
(zz:load-path "site-lisp/popup")

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat zzemacs-path "/site-lisp/auto-complete/dict"))
(ac-config-default)

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline  'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")


(provide 'ac-setting)

;;;; ac-setting.el --- end here
