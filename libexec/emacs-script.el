#!/usr/bin/emacs --script
;;;http://ergoemacs.org/emacs/emacs.html

(message "emacs script start ...")
(message "system-type:%s system-name:%s" system-type system-name)

;;print argument is setting
(when argv
  (message "input argument are %s" argv))


(message "emacs script end ...")
