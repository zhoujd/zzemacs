;;;; prog-setting.el --- program common file

(zz-load-path "site-lisp")

;;;$find -type f -name Makefile | xargs grep DIRVER_NAME
;;;$find -type f -name Makefile -exec grep -n DIRVER_NAME {} NUL;
;;;$find . -iregex .*\.el$ | xargs etags
(when-ms-windows    
 (setq find-program "\"find.exe\"")
 (setq grep-program "\"grep.exe\""))

(setq grep-find-use-xargs t)

;;develop setting for tags path etc.
(defvar zz-dev-set-file (format "%s/.emacs.d/temp-setting.el"
                                (getenv "HOME"))
  "temp project setting")
(when (file-exists-p zz-dev-set-file)
  (load-file zz-dev-set-file))

;;generate temp-setting.el
(defun my:create-file (fpath content)
  "Process the file at path FPATH ..."
  (let ((tmp-buf-name (file-name-nondirectory fpath)))
    (set-buffer (get-buffer-create tmp-buf-name))  
    (goto-char 0)
    (dolist (item content)
      (insert item)
      (insert "\n"))
    (write-file fpath)
    (kill-buffer tmp-buf-name)))

(defconst my:temp-template
  '(
    ";;;; temp-setting.el --- program temp file"
    ""
    ";; set project direcitory list"
    "(setq my:proj-list"
    "      '("
    "        \"/usr/include\""
    "        \"/opt/intel/mediasdk/include\""
    "        ))"
    ""
    ";; create etags & cscope"
    ";(my:create-proj-etags)"
    ";(my:create-proj-cscope)"
    ""
    ";; tags project setting"
    "(setq tags-table-list"
    "      '("
    "        \"~/work/tag\""
    "        ))"
    ""
    ";; cscope project setting"
    "(setq cscope-initial-directory"
    "      \"~/work/tag\")"
    ""
    "(mapc #'zz-add-os-path"
    "      '("
    "        \"~/work/script\""
    "        ))"
    ""
    "(mapc #'zz-add-lib-path"
    "      '("
    "        \"~/work/lib\""
    "        ))"
    ""
    ";; project key setting"
    ";(execute-set-key f4-p-map \"f\" \"firefox\" '(\"firefox\" \"http://www.baidu.com\"))"
    ))

(defun my:temp-setting ()
  "Create ~/.emacs.d/temp-setting.el"
  (interactive)
  (let ((path zz-dev-set-file))
    (if (file-exists-p path)
        (progn
         (find-file path)
         (message "open %s successful." path))
        (progn
         (my:create-file path my:temp-template)
         (message "create %s successful." path))
      )))

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


(defun my:newline-indents ()
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
  (add-hook hook (function my:newline-indents)))

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
(defun etags-select-get-tag-files ()
  "Get tag files."
  (if etags-select-use-xemacs-etags-p
      (buffer-tag-table-list)
    (mapcar 'tags-expand-table-name tags-table-list)
    (tags-table-check-computed-list)
    tags-table-computed-list))

(require 'etags-table)
(require 'etags-stack)

;;make ctags
(defun my:gen-ctags-cmd (dir-name)
  (format "ctags %s -f %s/TAGS -e -R %s"
          dir-name (directory-file-name dir-name)))

(defun my:create-ctags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (run-command-sort (my:gen-ctags-cmd dir-name)))

;;make etags
(defvar my:find-regex "*.[chCH] *.cc *.[ch]xx *.[ch]pp *.CC *.HH *.[ch]++")

(defun my:gen-find-parts (my-file-name)
  (setq my-find-parts "")
  (dolist (cell (split-string my-file-name))
    (setq my-find-parts (concat my-find-parts "-name \"" cell "\" -o ")))
  (setq my-find-parts (substring my-find-parts 0 -4)))

;(setq  my:c/c++-file-regex
;      (concat "-type f -name \"*.[hcHC]\" -print -or "
;              "-type f -name \"*.[hc]pp\" -print -or "
;              "-type f -name \"*.[hc]++\" -print -or "
;              "-type f -name \"*.[hc]xx\" -print "
;              ))

(defun my:gen-etags-cmd (dir-name)
  (format "%s %s -type f \\( %s \\) -print | etags -"
          find-program
          dir-name
          (my:gen-find-parts my:find-regex)))

(defun my:create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (run-command-sort (my:gen-etags-cmd dir-name)))

;;make cscope
; #!/bin/bash  
; find -type f | egrep "\.[hc]$|hh$|cc$|[hc]pp$|[hc]xx$|[hc]\+\+$">cscope.files
; cscope -bq -i ./csope.files
(defun my:gen-cscope-cmd (dir-name)
  (let ((files-path (concat default-directory "cscope.files")))
    (concat
     (format "%s %s -type f \\( %s \\) -print > %s;"
             find-program
             dir-name
             (my:gen-find-parts my:find-regex)
             files-path)
     (format "cscope -b -R -i %s" files-path)
     )))

(defun my:create-cscope (dir-name)
  "Create cscope file."
  (interactive "DDirectory: ")
  (run-command-sort (my:gen-cscope-cmd dir-name)))

;;creast etags/cscope for multi project
(defvar my:proj-list (list zzemacs-path) "project directory list")
(defun my:gen-proj-find-path (my:proj-list)
  (setq proj-path-parts "")
  (dolist (cell my:proj-list)
    (setq proj-path-parts (concat proj-path-parts cell " ")))
  (setq proj-path-parts (substring proj-path-parts 0 -1)))

(defun my:create-proj-etags ()
  (interactive)
  (my:create-etags (my:gen-proj-find-path my:proj-list)))

(defun my:create-proj-cscope ()
  (interactive)
  (my:create-cscope (my:gen-proj-find-path my:proj-list)))

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
(defvar my:rgrep-c-file-regex "*.[hc]")
(defun my:rgrep-c (term &optional dir)
  (interactive (list (completing-read "Search Term: " nil nil nil (thing-at-point 'word)))) 
  (grep-compute-defaults) 
  (let* ((dir (read-directory-name "Base directory: " nil default-directory t)))
    (rgrep term my:rgrep-c-file-regex dir)))

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

;;Add code review note
(defun my:add-code-review-note ()
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
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (defkeys-map yaml-mode-map
               ("\C-m" 'newline-and-indent))))

(provide 'prog-setting)

;;; prog-setting.el ends here
