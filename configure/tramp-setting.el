;;;; tramp-setting.el --- tramp configure

;;;tramp setting
;;Press C-l ido-reread-directory while in the process of finding a file.
;;C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file RET
;;C-x C-f /multi:ssh:foo@remote:ssh:bar@secret:~/.emacs
(require 'tramp)
(add-to-list 'tramp-methods
             '("sshz"
               (tramp-login-program        "ssh")
               (tramp-login-args           (("-l" "%u")
                                            ("-p" "%p")
                                            ("%c")
                                            ("-e" "none")
                                            ("-X")
                                            ("%h")
                                            ))
               (tramp-async-args           (("-q")))
               (tramp-remote-shell         "/bin/sh")
               (tramp-remote-shell-login   ("-l"))
               (tramp-remote-shell-args    ("-c"))
               (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                            ("-o" "UserKnownHostsFile=/dev/null")
                                            ("-o" "StrictHostKeyChecking=no")
                                            ("-o" "ForwardX11=yes")))
               (tramp-default-port         22)))
(tramp-set-completion-function "sshz" tramp-completion-function-alist-ssh)
(setq tramp-default-method "sshz")

;;tramp syntax: default' (default), `simplified' (ange-ftp like) or `separate' (XEmacs like)
;;default: /ssh:news@news.my.domain:/opt/news/etc
;;simplified: /news@news.my.domain:/opt/news/etc
;;separate: /[method/user@remotehost]/filename
(tramp-change-syntax 'default)

;;setup PS1 on remote
;;echo '[ $TERM == "dumb" ] && PS1="\u@\h \W\$ "' >> ~/.bashrc
(defun zz:tramp-ps1 ()
  (interactive)
  (comint-simple-send (get-buffer-process (current-buffer))
                      "PS1=\"\\u@\\h \\W\\$ \"")
  (message "setup tramp PS1 done"))

;;save PS1 to remote
(defun zz:tramp-ps1-save ()
  (interactive)
  (let ((content "
# PS1 for remote tramp
[ $TERM == \"dumb\" ] && PS1=\"\\u@\\h \\W\\$ \"")
        (file "~/.bashrc"))
    (comint-simple-send (get-buffer-process (current-buffer))
                        (format "echo '%s' >> %s" content file))
    (message "save tramp PS1 done")))


(provide 'tramp-setting)

;;;; tramp-setting.el --- end here
