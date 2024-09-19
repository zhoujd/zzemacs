;;;; lib-setting.el --- lib setting file
;;;

(zz/load-path "site-lisp")

;;dash
(require 'dash)

;;s
(require 's)

;;f
(require 'f)

;;crux
(require 'crux)

;;compat
(zz/load-path "site-lisp/compat")
(require 'compat)

;;hydra (already part of emacs)
(zz/load-path "site-lisp/emacs-legcy/hydra")
(require 'hydra)


(provide 'lib-setting)

;;; lib-setting.el ends here
