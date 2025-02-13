;;;; docker-setting.el

;;docker-tramp
;;https://www.emacswiki.org/emacs/TrampAndDocker
;;https://afsmnghr.github.io/emacs/tramp-with-ssh-and-docker/
;;sudo apt install docker-runc
;;C-x C-f /docker:container:/etc/passwd RET ;; simple use
;;C-x C-f /docker:user@container:/path/to/file
;;C-x C-f /ssh:user@remotehost|docker:user@container:/etc/passwd RET
(require 'docker-tramp)

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
(zz/load-path "site-lisp/docker")
(require 'docker)

;;https://github.com/Silex/docker-api.el
(zz/load-path "site-lisp/docker-api")
(require 'docker-api)

;;https://github.com/meqif/docker-compose-mode
(require 'docker-compose-mode)
(require 'docker-compose-mode-helpers)


(provide 'docker-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; docker-setting.el --- end here
