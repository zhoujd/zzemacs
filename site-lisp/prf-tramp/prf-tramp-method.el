;;; prf-tramp-method.el --- Utils to manipulate TRAMP method defs

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: tramp
;; URL: https://github.com/p3r7/prf-tramp
;; Package-Requires: ((dash "2.16.0")(s "1.11.0"))
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; REQUIRES

(require 'tramp)

(require 'dash)
(require 's)



;; UTILS: METHOD DEF

(defun prf/tramp/method/def (method)
  "Return definition of METHOD as defined in `tramp-methods'."
  (--first (string= (car it) method)
           tramp-methods))



;; UTILS: METHOD LOGIN EXEC

(defun prf/tramp/method/def/login-exec (method)
  (cadr
   (--first
    (eq (car it) 'tramp-login-program)
    (cdr (prf/tramp/method/def method)))))

(defun prf/tramp/method-def/with-login-exec (tramp-method-def login-exec)
  (let ((method-name (car tramp-method-def))
        (method-def-args (cdr tramp-method-def)))
    (cons method-name
          (-map-when
           (lambda (e) (equal (car e) 'tramp-login-program))
           (lambda (_e) `(tramp-login-program ,login-exec))
           method-def-args))))



;; UTILS: METHOD LOGIN ARGS

(defun prf/tramp/method/login-args (method)
  (cadr
   (--first
    (eq (car it) 'tramp-login-args)
    (cdr (prf/tramp/method/def method)))))

(defun prf/tramp/method/def/login-args/flattened (login-args host user port)
  (--map
   (s-replace-all
    `(("%h" . ,host)
      ("%u" . ,user)
      ("%p" . ,port))
    it)
   (-flatten login-args)))

(defun prf/tramp/method/flattened-login-args (method host user port)
  (prf/tramp/method/def/login-args/flattened (prf/tramp/method/login-args method) host user port))



;; UTILS: ENRICH LOGIN / COPY ARGS

(defun prf/tramp/method/def/some-args/with-cert (some-args cert-arg cert)
  "Returns enriched tramp def SOME-ARGS with certificate arg.
SOME-ARGS can be of type `tramp-login-args' or `tramp-copy-args'"
  (let ((args-type (car some-args))
        (args (car (cdr some-args))))
    (add-to-list 'args `(,cert-arg ,(concat "\"" cert "\"")))
    `(,args-type ,args)))

(defun prf/tramp/method/def/with-cert-in-some-args (tramp-method-def args-type cert-arg cert)
  "Returns copy of TRAMP-METHOD-DEF with certificate arg added to ARGS-TYPE.
ARGS-TYPE can be `tramp-login-args' or `tramp-copy-args'."
  (let ((method-name (car tramp-method-def))
        (method-def-args (cdr tramp-method-def)))
    (cons method-name
          (-map-when
           (lambda (e) (equal (car e) args-type))
           (lambda (e) (prf/tramp/method/def/some-args/with-cert e cert-arg cert))
           method-def-args))))

(defun prf/tramp/method/def/with-cert-in-args (tramp-method-def cert-arg cert)
  "Returns copy of TRAMP-METHOD-DEF enriched with certificate arg.
Certificate arg gets added to both 'tramp-login-args and 'tramp-copy-args."
  (-> tramp-method-def
      (prf/tramp/method/def/with-cert-in-some-args 'tramp-login-args cert-arg cert)
      (prf/tramp/method/def/with-cert-in-some-args 'tramp-copy-args cert-arg cert)))

(defun prf/tramp/method/def-with-cert-in-args (method cert-arg cert)
  "Return definition of METHOD as defined in `tramp-methods', enriched with certificate arg.
Certificate arg gets added to both 'tramp-login-args and 'tramp-copy-args."
  (prf/tramp/method/def/with-cert-in-args (prf/tramp/method/def method) cert-arg cert))

(defun prf/tramp-methods/with-some-with-cert-in-args (some-methods cert-arg cert)
  "Returns copy of `tramp-methods' with list of SOME-METHODS enriched with certificate."
  (-map-when
   (lambda (e) (member (car e) some-methods))
   (lambda (e) (prf/tramp/method/def/with-cert-in-args e cert-arg cert))
   tramp-methods))




(provide 'prf-tramp-method)

;;; prf-tramp-method.el ends here.
