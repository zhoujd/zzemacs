;;;perl programme setting

(zz-load-path "site-lisp/pde/lisp")
(require 'pde-load)

(defalias 'perl-mode 'cperl-mode)

;;;Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;;;perl code style
(defun zz/perl-pde-perl-mode-hook ()
  (abbrev-mode t)
  (hs-minor-mode t)
  (setq cperl-lazy-help-time 2)
  (cperl-lazy-install)
  (add-to-list 'cperl-style-alist
               '("PDE"
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
  (cperl-set-style "PDE"))

(add-hook 'cperl-mode-hook 'zz/perl-pde-perl-mode-hook)

;;;perl completing
(add-hook  'cperl-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t ) ; no error whatever auto-complete.el is not installed.
	       	   (require 'perl-completion nil t)
               (auto-complete-mode )
               (perl-completion-mode )
               (hs-minor-mode )
               (make-variable-buffer-local 'ac-sources)
               (setq ac-sources
                     '(ac-source-perl-completion
                       ac-source-yasnippet
                       ac-source-abbrev
                       ac-source-words-in-buffer
                       ac-source-files-in-current-dir
                       ac-source-filename
                       )))))

(provide 'perl-setting)

;; perl-setting.el end here
