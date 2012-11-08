;;;; prog-setting.el --- program common file

;;; project setting sample
;;;=============================================================================
;; set project direcitory list
;(setq proj-list '("~/zzsawfish/" "~/zzemacs/"))
;
;; call below function to create etags
;(create-proj-etags)
;; call below function to create cscope
;(create-proj-cscope)
;;
;; tags project setting
;(setq tags-table-list '("~/work/TAGS"))
;
;; cscope project setting 
;(setq cscope-database-regexps '(("~/work/"  (t ("-q" "-d")) t)))
;
;; add to PATH
;(setq add-path-list '("~/study/script"))
;(mapcar 'zz-add-os-path add-path-list)
;
;(setenv "LD_LIBRARY_PATH" (concat "~/work/lib"
;                                  path-separator (getenv "LD_LIBRARY_PATH")))
;;;=============================================================================

;; holding
(require 'hideshow)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(defun newline-indents ()
  "Bind Return to `newline-and-indent' in the local keymap."
  (local-set-key "\C-m" 'newline-and-indent))

;; Tell Emacs to use the function above in certain editing modes.
(add-hook 'lisp-mode-hook             (function newline-indents))
(add-hook 'emacs-lisp-mode-hook       (function newline-indents))
(add-hook 'lisp-interaction-mode-hook (function newline-indents))
(add-hook 'scheme-mode-hook           (function newline-indents))
(add-hook 'c-mode-hook                (function newline-indents))
(add-hook 'c++-mode-hook              (function newline-indents))
(add-hook 'java-mode-hook             (function newline-indents))
(add-hook 'cperl-mode-hook            (function newline-indents))
(add-hook 'php-mode-hook              (function newline-indents))

;;auto complete
(require 'cedet)
;; speed bar
(require 'semantic/sb)

;; Helper tools.
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))

;; smart complitions
(require 'semantic/ia)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

;; Semantic DataBase
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))

(global-ede-mode t)
(semantic-mode t)

;; semantic-ia-fast-jump fixed
(defadvice push-mark (around semantic-mru-bookmark activate)
  "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
  (semantic-mrub-push semantic-mru-bookmark-ring
                      (point)
                      'mark)
  ad-do-it)

;; hippie-try-expand settings
(setq hippie-expand-try-functions-list
      '(
        yas/hippie-try-expand
        semantic-ia-complete-symbol
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs))


(defun my-indent-or-complete ()
   (interactive)
   (if (looking-at "\\>")
      (hippie-expand nil)
      (indent-for-tab-command)))

;; Speed Bar Setting
(setq speedbar-update-speed 3)
(setq speedbar-use-images nil)  ;; clean face, :)
;;show unknows files
(setq speedbar-show-unknown-files t)

;;sr-speedbar
(require 'sr-speedbar)
(setq sr-speedbar-width  30)
(setq sr-speedbar-max-width 60)

;; his-speedbar-no-separate-frame
(defconst his-speedbar-buffer-name "SpeedBar")
(defun his-speedbar-no-separate-frame ()
  (interactive)
  (when (not (buffer-live-p speedbar-buffer))
    (setq speedbar-buffer (get-buffer-create his-speedbar-buffer-name)
          speedbar-frame (selected-frame)
          dframe-attached-frame (selected-frame)
          speedbar-select-frame-method 'attached
          speedbar-verbosity-level 0
          speedbar-last-selected-file nil)
    (set-buffer speedbar-buffer)
    (speedbar-mode)
    (speedbar-reconfigure-keymaps)
    (speedbar-update-contents)
    (speedbar-set-timer 1)
    (make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook
              (lambda () (when (eq (current-buffer) speedbar-buffer)
                           (setq speedbar-frame nil
                                 dframe-attached-frame nil
                                 speedbar-buffer nil)
                           (speedbar-set-timer nil)))))
  (set-window-buffer (selected-window)
                     (get-buffer his-speedbar-buffer-name)))

;; auto complete
(zz-load-path "site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat zzemacs-path "site-lisp/auto-complete/dict"))
(ac-config-default)

;; complete
(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (defun ac-next-or-next-line (arg)
    (interactive "p")
    (if (/= (length ac-candidates) 1)
        (ac-next)
      (ac-abort)
      (next-line arg)))
  (defun ac-previous-or-previous-line (arg)
    (interactive "p")
    (if (/= (length ac-candidates) 1)
        (ac-previous)
      (ac-abort)
      (previous-line arg)))
  
  (setq ac-use-menu-map t)
  ;; Default settings
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)

  (define-key ac-completing-map "\t" 'ac-complete)
  (define-key ac-completing-map "\r" nil)
  
  (set-face-background 'ac-candidate-face "lightgray")
  (set-face-underline  'ac-candidate-face "darkgray")
  (set-face-background 'ac-selection-face "steelblue")

  (dolist (command `(backward-delete-char-untabify delete-backward-char))
    (add-to-list 'ac-trigger-commands command))

  (defun ac-start-use-sources (sources)
    (interactive)
    (let ((ac-sources sources))
      (call-interactively 'ac-start)))

  (defvar ac-trigger-edit-commands
    `(self-insert-command
      delete-backward-char
      backward-delete-char
      backward-delete-char-untabify)
    "*Trigger edit commands that specify whether `auto-complete' should start or not when `ac-completing'.")
  )

;;YASNIPPET
(zz-load-path "site-lisp/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory (concat zzemacs-path "site-lisp/yasnippet/snippets"))

;; yasnippet show complete with  auto-complete
(defun ac-yasnippet-candidate ()
  (let ((table (yas/get-snippet-tables major-mode)))
    (if table
      (let (candidates (list))
            (mapcar (lambda (mode)
              (maphash (lambda (key value)
                (push key candidates))
              (yas/snippet-table-hash mode)))
            table)
        (all-completions ac-prefix candidates)))))

;load the etags-select.el source code
(require 'etags-select)
(defun etags-select-get-tag-files ()
  "Get tag files."
  (if etags-select-use-xemacs-etags-p
      (buffer-tag-table-list)
    (mapcar 'tags-expand-table-name tags-table-list)
    (tags-table-check-computed-list)
    tags-table-computed-list))

;(require 'etags-table)
(require 'etags-stack)

(global-set-key "\M-." 'etags-select-find-tag)

;;make ctags
(defun gen-ctags-cmd (dir-name)
  (format "ctags %s -f %s/TAGS -e -R %s"
          dir-name (directory-file-name dir-name)))

(defun create-ctags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (async-shell-command (gen-ctags-cmd dir-name)))

;;make etags
(setq my-find-regex "*.[chCH] *.cc *.[ch]xx *.[ch]pp *.CC *.HH *.[ch]++")

(defun gen-find-parts (my-file-name)
  (setq my-find-parts "")
  (dolist (cell (split-string my-file-name))
    (setq my-find-parts (concat my-find-parts "-name \"" cell "\" -o ")))
  (setq my-find-parts (substring my-find-parts 0 -4)))

;(setq  my-c/c++-file-regex
;      (concat "-type f -name \"*.[hcHC]\" -print -or "
;              "-type f -name \"*.[hc]pp\" -print -or "
;              "-type f -name \"*.[hc]++\" -print -or "
;              "-type f -name \"*.[hc]xx\" -print "
;              ))

(defun gen-etags-cmd (dir-name)
  (format "find %s -type f \\( %s \\) -print | etags -"
          dir-name (gen-find-parts my-find-regex)))

(defun create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (async-shell-command (gen-etags-cmd dir-name)))

;;make cscope
; #!/bin/bash  
; find -type f | egrep "\.[hc]$|hh$|cc$|[hc]pp$|[hc]xx$|[hc]\+\+$">cscope.files
; cscope -bq -i ./csope.files
(defun gen-cscope-cmd (dir-name)
  (concat
   (format "find %s -type f \\( %s \\) -print > %s/cscope.files;"
           dir-name (gen-find-parts my-find-regex)  default-directory)
   (format "cscope -b -R -q -i %s/cscope.files" default-directory)
   ))

(defun create-cscope (dir-name)
  "Create cscope file."
  (interactive "DDirectory: ")
  (async-shell-command (gen-cscope-cmd dir-name)))

;;creast etags/cscope for multi project
(defvar proj-list (list zzemacs-path) "project directory list")
(defun gen-proj-find-path (proj-list)
  (setq proj-path-parts "")
  (dolist (cell proj-list)
    (setq proj-path-parts (concat proj-path-parts cell " ")))
  (setq proj-path-parts (substring proj-path-parts 0 -1)))

(defun create-proj-etags ()
  (interactive)
  (create-etags (gen-proj-find-path proj-list)))

(defun create-proj-cscope ()
  (interactive)
  (create-cscope (gen-proj-find-path proj-list)))

;;add bat mode support
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

;; sawfish mode settings
;; load the first sawfish.el or sawfish.elc file found in the load-path
(require 'sawfish)
;; this tells emacs to automatically activate the sawfish-mode whenever open
;; file with "sawfishrc" or "jl" (John Lisp) suffix
(add-to-list 'auto-mode-alist '(".*sawfishrc\\'" . sawfish-mode ))
(add-to-list 'auto-mode-alist '(".*\\.jl\\'"     . sawfish-mode ))

;; for mysql
;; show output on windows in buffer
(setq sql-mysql-options '("-C" "-t" "-f" "-n"))

;; truncate lines for long tables
(add-hook 'sql-interactive-mode-hook
 (function (lambda ()
            (setq truncate-lines t))))

(setq auto-mode-alist
 (append
  (list
   ;; insert entries for other modes here if needed.
   (cons "\\.sql$" 'sql-mode))
  auto-mode-alist))

(add-hook 'sql-mode-hook 'font-lock-mode)

;;gud setting
;(require 'gdb-ui)
(defun gud-break-remove ()
  "Set/clear breakpoin."
  (interactive)
  (save-excursion
    (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
        (gud-remove nil)
      (gud-break nil))))

(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (dolist (buffer '(gdba gdb-stack-buffer gdb-breakpoints-buffer
                         gdb-threads-buffer gdb-inferior-io
                         gdb-registers-buffer gdb-memory-buffer
                         gdb-locals-buffer gdb-assembler-buffer))
    (when (gdb-get-buffer buffer)
      (let ((proc (get-buffer-process (gdb-get-buffer buffer))))
        (when proc (set-process-query-on-exit-flag proc nil)))
      (kill-buffer (gdb-get-buffer buffer)))))

;;SVN Support
;(require 'psvn)
;(require 'vc-svn)

;;Git Support
(zz-load-path "site-lisp/git-emacs")
(require 'git-emacs-autoloads)

;;rgrep for c/c++
(setq my-c-file-regex "*.[hc]")
(defun my-c-rgrep (term &optional dir)
  (interactive (list (completing-read "Search Term: " nil nil nil (thing-at-point 'word)))) 
  (grep-compute-defaults) 
  (let* ((dir (read-directory-name "Base directory: " nil default-directory t)))
    (rgrep term my-c-file-regex dir)))

;;self set for rgrep
(require 'grep)
(add-to-list 'grep-files-aliases
             '("hhcc" .  "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
             )


(add-hook 'gdb-mode-hook
          '(lambda ()
            (define-key gud-mode-map (kbd "C-c M-o") 'clear-input)))


(provide 'prog-setting)

;;; c-setting.el ends here
