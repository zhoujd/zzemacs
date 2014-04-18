;;;; Document setting

;;markdown setting
;;http://http://johnmacfarlane.net/pandoc/
(zz-load-path "site-lisp/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command (format
                          "perl %s/bin/markdown.pl | perl %s/bin/smartypants.pl"
                          zzemacs-path
                          zzemacs-path)))

(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))

;;org-mode setting
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
