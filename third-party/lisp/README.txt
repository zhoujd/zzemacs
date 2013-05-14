1. Quicklisp makes it easy to get started with a rich set of community-developed Common Lisp libraries. 
$ curl -O http://beta.quicklisp.org/quicklisp.lisp

* (load "quicklisp")
* (setq quicklisp-quickstart:*proxy-url* "http://host:port")
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

3. Getting started with ASDF
http://common-lisp.net/~mmommer/asdf-howto.shtml

4. Some lisp git repo
git clone https://github.com/feeley/gambit.git 
git clone http://git.code.sf.net/p/ecls/ecl ecls-ecl
git clone http://git.code.sf.net/p/ecls/ecl-doc ecls-ecl-doc

