;;;this mail setting
;;;http://ftp.isu.edu.tw/pub/Linux/LDP/LDP/LGNET/109/simpson.html
;;;VM is not part of the standard GNU Emacs lisp packages so it may be necessary for you to install VM on your own. 
;;;Check your Linux distribution's available packages to see if it can be installed via rpm, apt-get, or whatever tool is appropriate.
;;;sudo apt-get install vm

(add-to-list 'load-path "/usr/share/emacs23/site-lisp/vm")

(autoload 'vm "vm" "Start VM on your primary inbox." t)
(autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
(autoload 'vm-mail "vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)

(setq user-full-name "Your Full Name"
      mail-from-style 'angles
      user-mail-address "you@somewhere.on.the.internet"
      mail-default-reply-to user-mail-address)     

(setq vm-mutable-windows t
      vm-mutable-frames nil)

(setq vm-pop-folder-alist
      '(
        ("pop:your.pop3.server:110:pass:remote username:*" "identifier")
        ("pop:another.pop.server:110:pass:remote username:*" "unique id")
      ))

(add-hook 'dired-load-hook
          (lambda ()
             (setq dired-bind-vm t)))

