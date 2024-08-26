;;; tramp remote command

;;https://adamoudad.github.io/posts/emacs/remote-command-ssh/
(connection-local-set-profile-variables
 'remote-bash
 '((shell-file-name . "/bin/bash")
   (shell-command-switch . "-c")
   (shell-interactive-switch . "-i")
   (shell-login-switch . "-l")))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh" :machine "remote")
 'remote-bash)

(let ((default-directory (expand-file-name "/ssh:remote:~/"))
      (process-environment '("MYVAR=foobar")))
  (with-connection-local-variables
   (shell-command-to-string "echo Hi! I am on $HOSTNAME. Also MYVAR is set to $MYVAR.")
   ))


(provide 'tramp-remote-command)
