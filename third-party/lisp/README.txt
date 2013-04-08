1. Quicklisp makes it easy to get started with a rich set of community-developed Common Lisp libraries. 
$ curl -O http://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load quicklisp.lisp
This is SBCL 1.0.42.52, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.

  ==== quicklisp quickstart loaded ====

    To continue, evaluate: (quicklisp-quickstart:install)

* (quicklisp-quickstart:install)
* (ql:system-apropos "vecto")
* (ql:quickload "vecto")
* (ql:add-to-init-file)

2. Buildapp - Create executables with SBCL
Buildapp is an application for SBCL that configures and saves an executable Common Lisp image.
It is similar to cl-launch and hu.dwim.build.
Buildapp is available under a BSD-style license.

http://www.xach.com/lisp/buildapp/
Download shortcut:
http://www.xach.com/lisp/buildapp.tgz

$ buildapp \
    --eval '(defun main (argv) (declare (ignore argv)) (write-line "Hello, world"))' \
    --entry main \
    --output hello-world
$ ./hello-world
Hello, world

