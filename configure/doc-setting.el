;;;; Document setting

;;markdown setting
;;http://http://johnmacfarlane.net/pandoc/
(zz:load-path "site-lisp/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;set markdow command
(setq markdown-command
      (if (executable-find "markdown")
          (concat "markdown")
          (concat "perl " zzemacs-path "/libexec/markdown.pl | "
                  "perl " zzemacs-path "/libexec/smartypants.pl")))

;;github markdown preview
(zz:load-path "site-lisp/github-markdown-preview")
(require 'github-markdown-preview)

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

(defun zz:org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;;highlight code in code block
(setq org-src-fontify-natively t)


(provide 'doc-setting)

;;; doc-setting.el
