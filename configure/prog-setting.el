;;;; prog-setting.el --- program common file

(zz:load-path "site-lisp")

;;;$find -type f -name Makefile | xargs grep DIRVER_NAME
;;;$find -type f -name Makefile -exec grep -n DIRVER_NAME {} NUL;
;;;$find . -iregex .*\.el$ | xargs etags
(setq grep-find-use-xargs t)

;;develop setting for tags path etc.
(defvar zz:dev-set-file (format "%s/.emacs.d/temp-setting.el"
                                (getenv "HOME"))
  "temp project setting")

;;generate temp-setting.el
(defun zz:create-file (fpath content)
  "Process the file at path FPATH ..."
  (let ((tmp-buf-name (file-name-nondirectory fpath)))
    (set-buffer (get-buffer-create tmp-buf-name))
    (goto-char 0)
    (dolist (item content)
      (insert item)
      (insert "\n"))
    (write-file fpath)
    (kill-buffer tmp-buf-name)))

(defvar zz:proj-list (list zzemacs-path)
  "project directory list")
(defvar zz:tag-root (format "%s/.emacs.d/tags/"
                            (getenv "HOME"))
  "tag directory root")

(unless (file-exists-p zz:tag-root)
  (make-directory zz:tag-root))

;;temp setting template
(defconst zz:temp-template
  '(
    ";;;; temp-setting.el --- program temp file"
    ""
    ";; set project direcitory list"
    "(setq zz:proj-list '("
    "                     \"/usr/include\""
    "                     \"~/work/project-1\""
    "                     \"~/work/project-2\""
    "                     ))"
    ""
     "(mapc #'zz:add-os-path"
    "      '("
    "        \"~/work/script\""
    "        ))"
    ""
    "(mapc #'zz:add-lib-path"
    "      '("
    "        \"~/work/lib\""
    "        ))"
    ""
    ";; project key setting"
    ";(zz:exec-key f4-p-map \"f\" zz:firefox '(\"firefox\" \"http://www.baidu.com\"))"
    ))

(defun zz:temp-setting ()
  "Create ~/.emacs.d/temp-setting.el"
  (interactive)
  (let ((path zz:dev-set-file))
    (if (file-exists-p path)
        (progn
         (find-file path)
         (message "open %s successful." path))
        (progn
         (zz:create-file path zz:temp-template)
         (message "create %s successful." path))
      )))

(defun zz:temp-delete ()
  "delete ~/.emacs.d/temp-setting.el"
  (interactive)
  (let ((path zz:dev-set-file))
    (when (file-exists-p path)
      (delete-file path)
      (message "delete %s successful." path))
      ))

;; holding
(require 'hideshow)

(defun zz:newline-indents ()
  "Bind Return to `newline-and-indent' in the local keymap."
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key [ret] 'newline-and-indent))

;; Tell Emacs to use the function above in certain editing modes.
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

;;load the etags-select.el source code
(require 'etags-select)
(require 'etags-table)
(require 'etags-stack)
(let ((tag-path (concat zz:tag-root "TAGS")))
  (when (not (member tag-path  tags-table-list))
    (push tag-path tags-table-list)))

(defun zz:gen-ctags-cmd (dir-name)
  (format "ctags %s -f %s/TAGS -e -R %s"
          dir-name (directory-file-name dir-name)))

(defun zz:create-ctags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (zz:run-command (zz:gen-ctags-cmd dir-name)))

(defvar zz:find-regex "*.[chly] *.[ch]xx *.[ch]pp *.cc *.hh ")
(defun zz:gen-find-parts (file-regex)
  (let ((zz:find-parts ""))
    (dolist (cell (split-string file-regex))
      (setq zz:find-parts (concat zz:find-parts "-iname \"" cell "\" -o ")))
    (setq zz:find-parts (substring zz:find-parts 0 -4))))

(defun zz:gen-etags-cmd (dir-name)
  (concat
   (format "rm -f %s;"
           (concat default-directory "TAGS"))
   (format "%s %s -type f \\( %s \\) -print | etags -"
           find-program
           dir-name
           (zz:gen-find-parts zz:find-regex))))

;;https://www.emacswiki.org/emacs/TagsFile
(defun zz:create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (if (executable-find "etags")
      (zz:run-command (zz:gen-etags-cmd dir-name))
      (message "no etags, please install it")))

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
(defun zz:gen-cscope-cmd (dir-name)
  (let ((files-path (concat default-directory "cscope.files"))
        (out-path (concat default-directory "cscope.out")))
    (concat
     (format "rm -f %s;" (concat default-directory "cscope.*"))
     (format "%s %s \\( %s \\) \\( %s \\) \\( %s \\) -print | grep -v \" \" > %s;"
             find-program
             dir-name
             "-not -path '*/\.git/*'"
             "-type f -a -not -type l"
             (zz:gen-find-parts zz:find-regex)
             files-path)
     (format "cscope -b -R -q -k -i %s -f %s"
             files-path
             out-path)
     )))

(defun zz:create-cscope (dir-name)
  "Create cscope file."
  (interactive "DDirectory: ")
  (if (executable-find "cscope")
      (zz:run-command (zz:gen-cscope-cmd dir-name))
      (message "no cscope, please install it")))

;;creast etags/cscope for multi project
(defun zz:gen-proj-find-path (proj-list)
  (let ((proj-path-parts ""))
    (dolist (cell proj-list)
      (setq proj-path-parts (concat proj-path-parts cell " ")))
    (substring proj-path-parts 0 -1)))

(defun zz:create-proj-etags ()
  (interactive)
  (zz:create-etags (zz:gen-proj-find-path zz:proj-list)))

(defun zz:create-proj-cscope ()
  (interactive)
  (zz:create-cscope (zz:gen-proj-find-path zz:proj-list)))

;; create etags to zz:tag-root
(defun zz:create-root-etags ()
  (interactive)
  (let ((default-directory zz:tag-root))
    (unless (file-exists-p zz:tag-root)
      (make-directory zz:tag-root))
    (zz:create-proj-etags)))

;; create cscope to zz:tag-root
(defun zz:create-root-cscope ()
  (interactive)
  (let ((default-directory zz:tag-root))
    (unless (file-exists-p zz:tag-root)
      (make-directory zz:tag-root))
    (zz:create-proj-cscope)))

(defun zz:rscope-autoinit-path (buffer)
  "Look the directory from zz:tag-root"
  (when (file-readable-p (concat zz:tag-root rscope-database-name))
    zz:tag-root))

;; set cscope tag root
(defun zz:set-root-cscope ()
  (interactive)
  (cscope-set-initial-directory zz:tag-root)
  (setq helm-cscope-search-dir-init (list (list zz:tag-root)))
  (add-hook 'rscope-autoinit-cscope-dir-hooks
            (function zz:rscope-autoinit-path))
  (message "Set cscope init directory: %s" zz:tag-root))

;; unset cscope tag root
(defun zz:unset-root-cscope ()
  (interactive)
  (cscope-unset-initial-directory)
  (setq helm-cscope-search-dir-init nil)
  (remove-hook 'rscope-autoinit-cscope-dir-hooks
               (function zz:rscope-autoinit-path))
  (message "Unset cscope init directory"))

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

;; truncate lines for long tables
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

;;meson mode
(require 'meson-mode)

;;Add code review note
(defun zz:add-code-review-note ()
  "Add note for current file and line number"
  (interactive)
  (let ((file-name (buffer-file-name))
        (file-line (line-number-at-pos)))
    (switch-to-buffer-other-window (get-buffer-create "NOTES"))
    (goto-char (point-min))
    (when (not (search-forward "-*- mode:compilation-shell-minor"
                               nil t))
      (compilation-shell-minor-mode 1)
      (insert "-*- mode:compilation-shell-minor -*-\n\n"))
    (goto-char (point-max))
    (if (/= (current-column) 0)
        (newline))
    (insert file-name ":" (number-to-string file-line) ": ")))

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
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-suppress-auto-error t)
(dolist (hook
         (list
          'prog-mode-hook
          'yaml-mode-hook))
  (add-hook hook 'highlight-indent-guides-mode))

;;lsp-mode
;;https://emacs-lsp.github.io/lsp-mode/
;;https://systemcrafters.net/emacs-from-scratch/build-your-own-ide-with-lsp-mode/
(zz:load-path "site-lisp/lsp-mode")
(require 'lsp-mode)
(require 'lsp-lens)
(require 'lsp-modeline)
(require 'lsp-headerline)
(setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'

;;load temp setting
(when (file-exists-p zz:dev-set-file)
  (load-file zz:dev-set-file))


(provide 'prog-setting)

;;; prog-setting.el ends here
