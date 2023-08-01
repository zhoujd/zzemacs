;;;; lib-setting.el --- lib setting file
;;;

(zz:load-path "site-lisp")

;;hydra
(zz:load-path "site-lisp/hydra")
(require 'hydra)

;;dash
(require 'dash)

;;s
(require 's)

;;f
(require 'f)

;;crux
(require 'crux)

;;compat
(zz:load-path "site-lisp/compat")
(require 'compat)


(provide 'lib-setting)

;;; lib-setting.el ends here
