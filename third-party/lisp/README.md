# Lisp relation resource
* Quicklisp
  makes it easy to get started with a rich set of community-developed
  Common Lisp libraries.
  
        $ curl -O http://beta.quicklisp.org/quicklisp.lisp

        * (load "quicklisp")
        * (setq quicklisp-quickstart:*proxy-url* "http://host:port")
        * (quicklisp-quickstart:install)
        * (ql:system-apropos "vecto")
        * (ql:quickload "vecto")
        * (ql:add-to-init-file)

        ;;; The following lines added by ql:add-to-init-file:
        #-quicklisp
        (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                               (user-homedir-pathname))))
             (when (probe-file quicklisp-init)
                   (load quicklisp-init)))

        * (setq ql:*proxy-url* "http://host:port")

* Buildapp - Create executables with SBCL
  Buildapp is an application for SBCL that configures and saves an executable Common Lisp image.
  It is similar to cl-launch and hu.dwim.build.
  Buildapp is available under a BSD-style license.
  
  [BuildApp]<http://www.xach.com/lisp/buildapp/>
  
  [Download shortcut]<http://www.xach.com/lisp/buildapp.tgz>

        $ buildapp \
            --eval '(defun main (argv) (declare (ignore argv)) (write-line "Hello, world"))' \
            --entry main \
            --output hello-world
        $ ./hello-world
        Hello, world

* Getting started with ASDF

  [ASDF-Howto]<http://common-lisp.net/~mmommer/asdf-howto.shtml>

        ;;; Add to .sbclrc as below
        (require :asdf)
        (mapc
         #'(lambda (path)
             (let ((full-path (merge-pathnames path (user-homedir-pathname))))
               (pushnew full-path asdf:*central-registry* :test #'equal)))
         '(
           "lisp/system/"
           ))

        ;;; Load system
        (asdf:operate 'asdf:load-op 'multiviewer)


* Some lisp git repo

        git clone https://github.com/sbcl/sbcl.git>
        git clone https://github.com/feeley/gambit.git 
        git clone http://git.code.sf.net/p/ecls/ecl ecls-ecl
        git clone http://git.code.sf.net/p/ecls/ecl-doc ecls-ecl-doc
        git clone https://github.com/stumpwm/stumpwm

* Some lisp url

  [Gambit-C]<http://www.iro.umontreal.ca/~gambit/download/gambit/v4.6/prebuilt/>
  
  [ECL]<http://sourceforge.net/projects/ecls/?source=directory>

  [My StumpWM Config]<http://www.mygooglest.com/fni/stumpwm.html>

* Design Patterns in Dynamic Languanges

  <http://norvig.com/design-patterns/>
