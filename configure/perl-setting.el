;;;;perl programe setting
;;;http://emacswiki.org/cgi-bin/wiki/PerlLanguage
;;debug perl: "alt-x perldb" "perl -d myscript.pl"
;;https://harding.edu/gfoust/reference/perldb.html

;;use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;;eval buffer by perl
(defun zz:perl-eval-buffer ()
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

(defun zz:cperl-mode-hook ()
  (setq cperl-hairy t) ;; Turns on most of the CPerlMode options
  (defkeys-map cperl-mode-map
    ((kbd "C-c C-c") 'zz:perl-eval-buffer)))

(add-hook 'cperl-mode-hook 'zz:cperl-mode-hook t)

;;perl sepia settings
;;http://cpansearch.perl.org/src/SEANO/Sepia-0.97/Sepia.html
;;http://repo.or.cz/w/sepia.git
(zz-load-path "site-lisp/sepia")
(setq sepia-perl5lib (list (concat zzemacs-path "/site-lisp/sepia/lib")))
(defalias 'perl-mode 'sepia-mode)
(defalias 'sepia     'sepia-repl)
(defalias 'run-perl  'sepia-repl)
(require 'sepia)

(defun zz:sepia-mode-hook ()
  (defkeys-map sepia-mode-map
    ([(tab)] 'sepia-indent-or-complete)))

(add-hook 'sepia-mode-hook 'zz:sepia-mode-hook t)

;;perl completing
(add-hook  'cperl-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
               (require 'perl-completion nil t)
               (auto-complete-mode t)
               (perl-completion-mode t)
               (hs-minor-mode t)
               (make-variable-buffer-local 'ac-sources)
               (setq ac-sources
                     '(
                       ;;ac-source-perl-completion
                       ac-source-yasnippet
                       ac-source-abbrev
                       ac-source-words-in-buffer
                       ac-source-files-in-current-dir
                       ac-source-filename
                       )))))

;;perl code style
(add-hook  'cperl-mode-hook
           (lambda ()
             (add-to-list
              'cperl-style-alist
              '("MYPerlStyle"
                (cperl-auto-newline                         . nil)
                (cperl-brace-offset                         . 0)
                (cperl-close-paren-offset                   . -4)
                (cperl-continued-brace-offset               . 0)
                (cperl-continued-statement-offset           . 0)
                (cperl-extra-newline-before-brace           . nil)
                (cperl-extra-newline-before-brace-multiline . nil)
                (cperl-indent-level                         . 4)
                (cperl-indent-parens-as-block               . t)
                (cperl-label-offset                         . -4)
                (cperl-merge-trailing-else                  . t)
                (cperl-tab-always-indent                    . t)))
             (cperl-set-style "MYPerlStyle")))


;;plsense
(require 'plsense)
(plsense-config-default)


(provide 'perl-setting)

;;;; perl-setting.el end here
