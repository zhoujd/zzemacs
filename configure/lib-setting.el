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

;;async
(zz/load-path "site-lisp/async")
(require 'async)


(provide 'lib-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; lib-setting.el ends here
