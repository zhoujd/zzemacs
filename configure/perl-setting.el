;;;;perl programe setting
;;;http://emacswiki.org/cgi-bin/wiki/PerlLanguage

;;Use cperl-mode
(defalias 'perl-mode 'cperl-mode)

;;Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(setq cperl-hairy t) ;; Turns on most of the CPerlMode options

(defun run-perl ()
  "run perl on buffer"
  (interactive)
  (let ((filename buffer-file-name)
  (cmd "")
  (oldbuf (current-buffer))
  (end (point-max)))
    (if filename
      (save-buffer)
        (save-excursion
          (setq filename (concat (getenv "tmp") "/temp.pl"))
          (set-buffer (create-file-buffer filename))
          (insert-buffer-substring oldbuf 1 end)
          (write-file filename)
          (kill-buffer (current-buffer))))
    (setq cmd (concat "perl -w " filename))
    (message "%s  ..." cmd)
    (shell-command cmd)))

(defun my-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-extra-newline-before-brace t)
  (define-key cperl-mode-map (kbd "C-c C-c") 'run-perl))

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook t)

;;Perl sepia settings
;;http://cpansearch.perl.org/src/SEANO/Sepia-0.97/Sepia.html
;;http://repo.or.cz/w/sepia.git
(zz-load-path "site-lisp/sepia")
(setq sepia-perl5lib (list (concat zzemacs-path "/site-lisp/sepia/lib")))
(defalias 'perl-mode 'sepia-mode)
(defalias 'sepia  'sepia-repl)
(require 'sepia)

(defun my-sepia-mode-hook ()
  (define-key sepia-mode-map [(tab)] 'sepia-indent-or-complete))

(add-hook 'sepia-mode-hook 'my-sepia-mode-hook t)


(provide 'perl-setting)

;;;; perl-setting.el end here
