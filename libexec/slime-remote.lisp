#!/usr/local/bin/sbcl --script

(write-line "start slime at port:7070")

;; auto start swank server
(require 'swank)
(defvar *swank-server-p* nil "swant server flag")

(unless *swank-server-p*
  (swank:create-server :dont-close t :port 7070)
  (setf swank:*use-dedicated-output-stream* nil)
  (setf *swank-server-p* t))
