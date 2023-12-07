;;;; prog-setting.el --- program common file

(zz:load-path "site-lisp")

;;;$find -type f -name Makefile | xargs grep DIRVER_NAME
;;;$find -type f -name Makefile -exec grep -n DIRVER_NAME {} NUL;
;;;$find . -iregex .*\.el$ | xargs etags
(setq grep-find-use-xargs t)

;;;https://stackoverflow.com/questions/5451381/stack-overflow-while-generating-tags-completion-table-in-emacs
;;auto reread tag file
(require 'etags-select)
(require 'etags-table)

;;holding
(require 'hideshow)

(defun zz:newline-indents ()
  "Bind Return to `newline-and-indent' in the local keymap."
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key [ret] 'newline-and-indent))

;;tell Emacs to use the function above in certain editing modes.
(dolist (hook
         (list
          'lisp-mode-hook
          'emacs-lisp-mode-hook
          'lisp-interaction-mode-hook
          'scheme-mode-hook
          'c-mode-common-hook
          'java-mode-hook
          'perl-mode-hook
          'python-mode-hook
          'php-mode-hook))
  (add-hook hook (function zz:newline-indents)))

;;sr-speedbar
(require 'sr-speedbar)
(setq speedbar-update-speed 3
      speedbar-use-images nil
      speedbar-show-unknown-files t
      sr-speedbar-width  30
      sr-speedbar-max-width 60
      sr-speedbar-right-side nil)

(defun zz:gen-ctags-cmd (dir)
  (format "ctags %s -f %s/TAGS -e -R %s"
          dir (directory-file-name dir)))

(defun zz:create-ctags (dir)
  "Create tags file."
  (interactive "DDirectory: ")
  (zz:run-command (zz:gen-ctags-cmd dir)))

(defvar zz:find-regex "*.[chly] *.[ch]xx *.[ch]pp *.cc *.hh ")
(defun zz:gen-find-parts (file-regex)
  (let ((zz:find-parts ""))
    (dolist (cell (split-string file-regex))
      (setq zz:find-parts (concat zz:find-parts "-iname \"" cell "\" -o ")))
    (setq zz:find-parts (substring zz:find-parts 0 -4))))

(defun zz:gen-etags-cmd (dir)
  (concat
   (format "rm -f %s;"
           (concat default-directory "TAGS"))
   (format "%s %s -type f \\( %s \\) -print | etags -"
           find-program
           dir
           (zz:gen-find-parts zz:find-regex))))

;;https://www.emacswiki.org/emacs/TagsFile
;;sudo apt install emacs-bin-common
(defun zz:create-etags (dir)
  "Create tags file."
  (interactive "DDirectory: ")
  (if (executable-find "etags")
      (zz:run-command (zz:gen-etags-cmd dir))
      (message "no etags, please install it")))

(defun zz:remote-etags ()
  "Create tags file."
  (interactive)
  (zz:run-command
   (concat
    (format "rm -f TAGS;")
    (format "%s -type f \\( %s \\) -print | etags -"
            find-program
            (zz:gen-find-parts zz:find-regex))
    )))

;;https://github.com/dkogan/xcscope.el
;;C-c s I     Create list and index
(require 'xcscope)
(setq cscope-option-kernel-mode t
      cscope-option-use-inverted-index t)
(cscope-setup)

;;https://github.com/rjarzmik/rscope
(require 'rscope)
(require 'rscope-nav)
(setq rscope-keymap-prefix (kbd "M-g s"))

;;make cscope
; #!/bin/bash
; find -type f -not -path '*/\.git/*'
; find -type f -not -path '*/\.svn/*'
; find -type f -not -path '*/\.*'
; find -type f | egrep "\.[hc]$|hh$|cc$|[hc]pp$|[hc]xx$|[hc]\+\+$">cscope.files
; cscope -bq -i ./cscope.files
(defun zz:gen-cscope-cmd (dir)
  (let ((files-path (concat default-directory "cscope.files"))
        (out-path (concat default-directory "cscope.out")))
    (concat
     (format "rm -f %s;" (concat default-directory "cscope.*"))
     (format "%s %s \\( %s \\) \\( %s \\) \\( %s \\) -print | grep -v \" \" > %s;"
             find-program
             dir
             "-not -path '*/\.git/*'"
             "-type f -a -not -type l"
             (zz:gen-find-parts zz:find-regex)
             files-path)
     (format "cscope -b -R -q -k -i %s -f %s"
             files-path
             out-path)
     )))

(defun zz:create-cscope (dir)
  "Create cscope file."
  (interactive "DDirectory: ")
  (if (executable-find "cscope")
      (zz:run-command (zz:gen-cscope-cmd dir))
      (message "no cscope, please install it")))

(defun zz:remote-cscope ()
  (interactive)
  (let ((files-path "cscope.files")
        (out-path "cscope.out"))
    (zz:run-command
     (concat
      (format "rm -f %s;" "cscope.*")
      (format "%s \\( %s \\) \\( %s \\) \\( %s \\) -print | grep -v \" \" > %s;"
              find-program
              "-not -path '*/\.git/*'"
              "-type f -a -not -type l"
              (zz:gen-find-parts zz:find-regex)
              files-path)
      (format "cscope -b -R -q -k -i %s -f %s"
              files-path
              out-path)
      ))))

;;add  mode support
(setq auto-mode-alist
   (append
    (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
    (list (cons "\\.[cC][mM][dD]$" 'bat-mode))
    (list (cons "\\.[gG][dD][bB]$" 'gdb-script-mode))
    (list (cons "\\.gdbinit$"      'gdb-script-mode))
    (list (cons "\\.[mM][aA][kK]$" 'makefile-mode))
    ;; For DOS init files
    ;(list (cons "CONFIG\\."   'bat-mode))
    ;(list (cons "AUTOEXEC\\." 'bat-mode))
    auto-mode-alist))

(autoload 'bat-mode "bat-mode"
    "DOS and WIndows BAT files" t)

;;for mysql
;;show output on windows in buffer
(setq sql-mysql-options '("-C" "-t" "-f" "-n"))

;;truncate lines for long tables
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq truncate-lines t)))

(setq auto-mode-alist
      (append
       (list
        ;; insert entries for other modes here if needed.
        (cons "\\.sql$" 'sql-mode))
       auto-mode-alist))

(add-hook 'sql-mode-hook 'font-lock-mode)

;;rgrep for c/c++
(defvar zz:rgrep-c-file-regex "*.[hc]")
(defun zz:rgrep-c (term &optional dir)
  (interactive (list (completing-read "Search Term: " nil nil nil (thing-at-point 'word))))
  (grep-compute-defaults)
  (let* ((dir (read-directory-name "Base directory: " nil default-directory t)))
    (rgrep term zz:rgrep-c-file-regex dir)))

;;javascript mode
(require 'js2-mode)

;;xmsi-mode for inputting math (Unicode) symbols.
(require 'xmsi-math-symbols-input)

;;cmake file support
(require 'cmake-mode)
(setq cmake-tab-width 4)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))
;;set lsp-mode or eglot
;;pip install cmake-language-server
;(add-hook 'cmake-mode-hook 'lsp-deferred t)

;;meson mode
(require 'meson-mode)

;;yaml-mode
(require 'yaml-mode)
(add-hook 'yaml-mode-hook
          (lambda ()
             (defkeys-map yaml-mode-map ("\C-m" 'newline-and-indent))))

;;nginx-mode
(require 'nginx-mode)
(add-hook 'nginx-mode-hook #'company-nginx-keywords)

;;plantum-mode
(require 'plantuml-mode)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;;ztree
(zz:load-path "site-lisp/ztree")
(require 'ztree)

;;ansible
(zz:load-path "site-lisp/ansible")
(require 'ansible)
(add-hook 'yaml-mode-hook (lambda () (ansible 1)))

;;https://github.com/krzysztof-magosa/company-ansible
(zz:load-path "site-lisp/company-ansible")
(require 'company-ansible)
(add-to-list 'company-backends 'company-ansible)

;;json
(require 'json-mode)

;;diffview
(require 'diffview)

;;protocol buffers
;;https://github.com/protocolbuffers/protobuf/blob/master/editors/protobuf-mode.el
(require 'protobuf-mode)

;;highlight-indent-guides
;;'fill, 'column, 'character, or 'bitmap
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'bitmap)
(setq highlight-indent-guides-suppress-auto-error t)
(dolist (hook
         (list
          'prog-mode-hook
          'yaml-mode-hook))
  (add-hook hook 'highlight-indent-guides-mode))

;;project-custom
(zz:load-path "elisp")
(require 'project-custom)


(provide 'prog-setting)

;;; prog-setting.el ends here
