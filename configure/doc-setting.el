;;;; Document setting

;;markdown setting
;;https://github.com/jrblevin/markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;The default command for markdown (~markdown~), doesn't support tables
;;(e.g. GitHub flavored markdown). Pandoc does, so let's use that.
(setq markdown-command "pandoc --from markdown --to html")
(setq markdown-command-needs-filename t)
(setq markdown-coding-system 'utf-8)

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
(require 'org-present)
(defun zz:org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun zz:org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (hide-mode-lines)
  (zz:org-present-prepare-slide))

(defun zz:org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (show-mode-lines)
  (org-remove-inline-images))

(defun zz:org-present-prev ()
  (interactive)
  (org-present-prev)
  (zz:org-present-prepare-slide))

(defun zz:org-present-next ()
  (interactive)
  (org-present-next)
  (zz:org-present-prepare-slide))

(add-hook 'org-present-mode-hook 'zz:org-present-hook)
(add-hook 'org-present-mode-quit-hook 'zz:org-present-quit-hook)

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

;;mermaid-mode
;;https://github.com/abrochard/mermaid-mode
(require 'mermaid-mode)
(setq mermaid-mmdc-location "docker")
(setq mermaid-flags "run -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:latest")

;;d2-mode
;;https://github.com/andorsk/d2-mode
(require 'd2-mode)
(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))


(provide 'doc-setting)

;;; doc-setting.el
