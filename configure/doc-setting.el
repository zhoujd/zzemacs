;;;; Document setting

;;markdown setting
;;http://http://johnmacfarlane.net/pandoc/
(zz-load-path "site-lisp/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;set markdow command
(setq markdown-command
       (if-ms-windows
        (progn
         (concat "markdown"))
        (progn
         (concat "perl " zzemacs-path "/libexec/markdown.pl | "
                 "perl " zzemacs-path "/libexec/smartypants.pl"))))

;;org-mode setting
;;http://orgmode.org/
(setq org-export-html-postamble nil)
(setq org-publish-project-alist
      '(("org"
         :base-directory "~/org/"
         :publishing-directory "~/public_html"
         :section-numbers nil
         :table-of-contents nil
         :style "<link rel=\"stylesheet\"
                 href=\"../other/mystyle.css\"
                 type=\"text/css\"/>")))


(provide 'doc-setting)

;;; doc-setting.el
