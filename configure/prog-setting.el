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
(when (file-exists-p zz:dev-set-file)
  (load-file zz:dev-set-file))

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

;;project list
(defvar zz:proj-list (list zzemacs-path) "project directory list")

;;temp setting template
(defconst zz:temp-template
  '(
    ";;;; temp-setting.el --- program temp file"
    ""
    ";; set project direcitory list"
    "(setq zz:proj-list '("
    "                     \"/usr/include\""
    "                     \"/opt/zach/include\""
    "                     ))"
    ""
    ";; tags project setting"
    "(setq tags-table-list '("
    "                        \"~/work/tag/TAGS\""
    "                        ))"
    ""
    ";; cscope project setting"
    "(setq cscope-initial-directory \"~/work/tag\")"
    ""
    ";; create etags & cscope"
    ";(zz:create-proj-etags)"
    ";(zz:create-proj-cscope)"
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
    ";(zz:execute-key f4-p-map \"f\" zz:firefox '(\"firefox\" \"http://www.baidu.com\"))"
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
(dolist (hook
         (list
          'java-mode-hook
          'c++-mode-hook
          'python-mode-hook
          'c-mode-hook
          'perl-mode-hook
          'php-mode-hook
          'emacs-lisp-mode-hook))
  (add-hook hook 'hs-minor-mode))

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

;;make ctags
(defun zz:gen-ctags-cmd (dir-name)
  (format "ctags %s -f %s/TAGS -e -R %s"
          dir-name (directory-file-name dir-name)))

(defun zz:create-ctags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (zz:run-command (zz:gen-ctags-cmd dir-name)))

;;make etags
(defvar zz:find-regex "*.[chCH] *.cc *.[ch]xx *.[ch]pp *.CC *.HH *.[ch]++")

(defun zz:gen-find-parts (file-name)
  (setq zz:find-parts "")
  (dolist (cell (split-string file-name))
    (setq zz:find-parts (concat zz:find-parts "-name \"" cell "\" -o ")))
  (setq zz:find-parts (substring zz:find-parts 0 -4)))

;(setq zz:c/c++-file-regex
;      (concat "-type f -name \"*.[hcHC]\" -print -or "
;              "-type f -name \"*.[hc]pp\" -print -or "
;              "-type f -name \"*.[hc]++\" -print -or "
;              "-type f -name \"*.[hc]xx\" -print "
;              ))

(defun zz:gen-etags-cmd (dir-name)
  (format "%s %s -type f \\( %s \\) -print | etags -"
          find-program
          dir-name
          (zz:gen-find-parts zz:find-regex)))

(defun zz:create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (if (executable-find "etags")
      (zz:run-command (zz:gen-etags-cmd dir-name))
      (message "no etags, please install it")))

;;make cscope
; #!/bin/bash  
; find -type f | egrep "\.[hc]$|hh$|cc$|[hc]pp$|[hc]xx$|[hc]\+\+$">cscope.files
; cscope -bq -i ./csope.files
(defun zz:gen-cscope-cmd (dir-name)
  (let ((files-path (concat default-directory "cscope.files")))
    (concat
     (format "%s %s -type f \\( %s \\) -print > %s;"
             find-program
             dir-name
             (zz:gen-find-parts zz:find-regex)
             files-path)
     (format "cscope -b -R -q -i %s" files-path)
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
(dolist (hook
         (list
          'prog-mode-hook
          'yaml-mode-hook))
  (add-hook hook 'highlight-indent-guides-mode))


(provide 'prog-setting)

;;; prog-setting.el ends here
