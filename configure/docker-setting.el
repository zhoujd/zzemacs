;;;; docker-setting.el

;;docker-tramp
;;C-x C-f /ssh:user@host|docker:
(require 'docker-tramp)
(defadvice tramp-completion-handle-file-name-all-completions
    (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we '
use strict;
$_ = <>;
m/^(.*)NAMES/ or die;
my $offset = length($1);
while(<>) {
  substr($_, 0, $offset, q());
  chomp;
  for(split m/\\W+/) {
    print qq($_:\n)
  }
}
'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
      ad-do-it))

;;dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;https://github.com/Silex/docker.el
(zz-load-path "site-lisp/docker")
(require 'docker)


(provide 'docker-setting)

;;;; docker-setting.el --- end here
