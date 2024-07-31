;;; prf-tramp-friendly-parsing.el --- Human-friendly TRAMP path contruction

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: tramp, rx
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
;; Utils to provide more user-friendly parsing capabilities than
;; default TRAMP.
;; For an example usage, see `prf/tramp/remote-shell'.
;;
;; For detailed instructions, please look at the README.md


;;; Code:



;; REQUIRES

(require 'tramp)
(require 'tramp-sh)

(require 'dash)
(require 's)



;; CONSTANTS: PERMISSIVE TRAMP PATH DISSECT

(defconst prf/tramp/rx/method
  `(line-start
    ,tramp-prefix-format
    (group (one-or-more (any "a-z" "A-Z" "0-9")))
    ;; (group ,tramp-method-regexp)
    ,tramp-postfix-method-format))

(defconst prf/tramp/rx/user-no-postfix
  `(
    (group (one-or-more (any "a-z" "A-Z" "0-9" ".")))
    ;; (group ,tramp-user-regexp)
    ))

(defconst prf/tramp/rx/user
  `(
    (group (one-or-more (any "a-z" "A-Z" "0-9" ".")))
    ;; (group ,tramp-user-regexp)
    ,tramp-postfix-user-format))

(defconst prf/tramp/rx/domain
  `(,tramp-prefix-domain-regexp
    (group ,tramp-domain-regexp)))

(defconst prf/tramp/rx/host
  `(
    (group (one-or-more (any "a-z" "A-Z" "0-9" "." "-")))
    ;; (group ,tramp-host-regexp)
    (zero-or-more ,tramp-postfix-host-format)))

(defconst prf/tramp/rx/port
  `(,tramp-prefix-port-format
    (group ,tramp-port-regexp)))

(defconst prf/tramp/rx/localname
  `(
    (group (zero-or-more anything))
    (group ,tramp-localname-regexp)
    ))



;; UTILS: rx WRAPPER

;; REVIEW: I'm afraid than doing this might induce a performance penalty.
;; but this makes things reusable
(defun prf/tramp/rx (args)
  "Call `rx' with list ARGS.
Doing this as `rx' is a macro and we cannot use `apply'."
  (eval (cons #'rx args)))



;; UTILS: PERMISSIVE TRAMP PATH DISSECT

(defun prf/tramp/get-method-from-path (path)
  ;; /METHOD:*
  (when (string-match (prf/tramp/rx prf/tramp/rx/method)
                      path)
    (match-string 1 path)))

(defun prf/tramp/get-user-from-path (path)
  ;; /METHOD:USER%DOMAIN@*
  (if (string-match (prf/tramp/rx (-concat prf/tramp/rx/method
                                           prf/tramp/rx/user-no-postfix
                                           prf/tramp/rx/domain
                                           `(,tramp-postfix-user-format)))
                    path)
      (match-string 2 path)
    ;; /METHOD:USER@*
    (if (string-match (prf/tramp/rx (-concat prf/tramp/rx/method
                                             prf/tramp/rx/user))
                      path)
        (match-string 2 path)
      ;; USER@*
      (when (string-match (prf/tramp/rx (-concat '(line-start)
                                                 prf/tramp/rx/user))
                          path)
        (match-string 1 path)))))

(defun prf/tramp/get-host-from-path (path)
  ;; *@*
  (if (s-contains? tramp-postfix-user-format path)
      ;; *USER@HOST[:*]
      (when (string-match (prf/tramp/rx (-concat prf/tramp/rx/user
                                                 prf/tramp/rx/host))
                          path)
        (match-string 2 path))
    (cond
     ;; /METHOD:HOST[:*]
     ((string-match (prf/tramp/rx (-concat prf/tramp/rx/method
                                           prf/tramp/rx/host))
                    path)
      (match-string 2 path))
     ;; HOST
     ((string-match (prf/tramp/rx (-concat '(line-start)
                                           prf/tramp/rx/host
                                           '(eol)))
                    path)
      (match-string 1 path))
     ;; TODO: HOST:* ?
     )))

(defun prf/tramp/get-localname-from-path (path)
  ;; /METHOD:USER@HOST:LOCALNAME, i.e. 2 : in the path
  (if (string-match (prf/tramp/rx (-concat prf/tramp/rx/method
                                           prf/tramp/rx/user
                                           prf/tramp/rx/host
                                           prf/tramp/rx/localname))
                    path)
      (match-string 4 path)
    (when (string-match (prf/tramp/rx (-concat '((one-or-more anything))
                                               `(,tramp-postfix-host-format)
                                               prf/tramp/rx/localname))
                        path)
      (match-string 1 path))))

(defun prf/tramp/path/dissect (path)
  "More permissive version of `tramp-dissect-file-name'.
Return a VEC.
Accepts input from `prf/tramp/remote-shell'."
  (if (file-remote-p path)
      (tramp-dissect-file-name path)
    (let ((method (prf/tramp/get-method-from-path path))
          (user (prf/tramp/get-user-from-path path))
          (host (prf/tramp/get-host-from-path path))
          (localname (prf/tramp/get-localname-from-path path)))
      (if (eq (length method) 0)
          (setq method tramp-default-method))
      (if (eq (length user) 0)
          (setq user tramp-default-user))
      (if (eq (length localname) 0)
          (setq localname "/"))
      `(tramp-file-name ,method ,user nil ,host nil ,localname))))




(provide 'prf-tramp-friendly-parsing)

;;; prf-tramp-friendly-parsing.el ends here.
