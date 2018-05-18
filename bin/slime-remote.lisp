;;;; slime-remote.lisp
;;; sbcl --load slime-remote.lisp
;;; http://cl-cookbook.sourceforge.net
;;; http://cl-cookbook.sourceforge.net/os.html

(write-line "Start slime at default port: 7070")

(defun my-command-line ()
  (or
   #+CLISP *args*
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun my-getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

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
