;;;; Document setting

;;markdown setting
;;http://johnmacfarlane.net/pandoc/
;;https://github.com/jrblevin/markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;The default command for markdown (~markdown~), doesn't support tables
;;(e.g. GitHub flavored markdown). Pandoc does, so let's use that.
(setq markdown-command "pandoc --from markdown --to html")
(setq markdown-command-needs-filename t)

;;github markdown preview
(zz:load-path "site-lisp/github-markdown-preview")
(require 'github-markdown-preview)

;;markdown Table of Contents
(require 'markdown-toc)
(custom-set-variables
 '(markdown-toc-header-toc-title "**Table of Contents**")
 '(markdown-toc-header-toc-start "<!-- customized start-->")
 '(markdown-toc-header-toc-end "<!-- customized end -->")
 '(markdown-toc-indentation-space 2))

(setq markdown-toc-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c m .") 'markdown-toc-follow-link-at-point)
        (define-key map (kbd "C-c m t") 'markdown-toc-generate-or-refresh-toc)
        (define-key map (kbd "C-c m d") 'markdown-toc-delete-toc)
        (define-key map (kbd "C-c m v") 'markdown-toc-version)
        map))

(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode -1)
            (markdown-toc-mode 1)
            ))

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

;;Github Flavored Markdown exporter for Org Mode
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;;https://github.com/rlister/org-present
(autoload 'org-present "org-present" nil t)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;;org plantuml
;;https://plantuml.com/emacs
;;Ubuntu: apt install plantuml
;;export e.g. with C-c C-e h o
(setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;;https://webonastick.com/emacs-lisp/hide-mode-line.el
(autoload 'hide-mode-line "hide-mode-line" nil t)

;;pandoc mode
;;apt install pandoc
(zz:load-path "site-lisp/pandoc-mode")
(zz:load-path "site-lisp/hydra")
(require 'pandoc-mode)

;;epub reading
;;https://depp.brause.cc/nov.el
(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(provide 'doc-setting)

;;; doc-setting.el
