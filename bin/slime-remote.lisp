;;; sbcl --load slime-remote.lisp

(write-line "Start slime at default port: 7070")

#+sbcl
(require 'swank)

#+clisp
(asdf:operate 'asdf:load-op 'swank)

(defvar *swank-server-p* nil "swant server flag")
(defvar *swant-server-port* 7070  "swant server port")

;; start swank server
(unless *swank-server-p*
  (swank:create-server :dont-close t
                       :port *swant-server-port*)
  (setf *swank-server-p* t))
