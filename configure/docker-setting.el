;;;; docker-setting.el

;;docker-tramp
;;https://www.emacswiki.org/emacs/TrampAndDocker
;;https://afsmnghr.github.io/emacs/tramp-with-ssh-and-docker/
;;sudo apt install docker-runc
;;C-x C-f /docker:container:/etc/passwd RET ;; simple use
;;C-x C-f /docker:user@container:/path/to/file
;;C-x C-f /ssh:user@remotehost|docker:user@container:/etc/passwd RET
(require 'docker-tramp)
(push (cons "docker"
            '((tramp-login-program "docker")
              (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
              (tramp-remote-shell "/bin/sh")
              (tramp-remote-shell-args ("-i") ("-c"))))
      tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
    (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\"
    returns a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw
              (shell-command-to-string
               "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
      ad-do-it))

;;dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;https://github.com/Silex/docker.el
(zz:load-path "site-lisp/docker")
(require 'docker)

;;https://github.com/chrisbarrett/kubernetes-el
(zz:load-path "site-lisp/kubernetes-el")
(require 'kubernetes)


(provide 'docker-setting)

;;;; docker-setting.el --- end here
