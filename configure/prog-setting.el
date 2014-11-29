;;;; prog-setting.el --- program common file

(zz-load-path "site-lisp")

;;;$find -type f -name Makefile | xargs grep DIRVER_NAME
;;;$find -type f -name Makefile -exec grep -n DIRVER_NAME {} NUL;
;;;$find . -iregex .*\.el$ | xargs etags
;;win32 find grep set
(when-ms-windows    
 (progn
   (setq find-program "\"find.exe\"")
   (setq grep-program "\"grep.exe\"")))

;;generate temp-setting.el
(defun my-create-file (fpath content)
  "Process the file at path FPATH ..."
  (let ((tmp-buf-name (file-name-nondirectory fpath)))
    (set-buffer (get-buffer-create tmp-buf-name))  
    (goto-char 0)
    (dolist (item content)
      (insert item)
      (insert "\n"))
    (write-file fpath)
    (kill-buffer tmp-buf-name)))

(defconst my-temp-setting
  '(
    ";;;; temp-setting.el --- program temp file"
    ";; set project direcitory list"
    "(setq proj-list '("
    "                  \"/usr/include\""
    "                  \"/opt/intel/mediasdk/include\""
    "                  ))"
    ""
    ";; call below function to create etags"
    ";(create-proj-etags)"
    ";; call below function to create cscope"
    ";(create-proj-cscope)"
    ""
    ";; tags project setting"
    "(setq tags-table-list '(\"~/work/TAGS\"))"
    ""
    ";; cscope project setting"
    "(setq cscope-database-regexps '((\"~/work/\"  (t (\"-q\" \"-d\")) t)))"
    ""
    ";; add to PATH"
    "(setq add-path-list '(\"~/study/script\"))"
    "(mapcar 'zz-add-os-path (reverse add-path-list))"
    ""
    ";(setenv \"LD_LIBRARY_PATH\" (concat \"~/work/lib\""
    ";        path-separator (getenv \"LD_LIBRARY_PATH\")))"
    ""
    ";; project key setting"
    ";(execute-set-key f4-p-map \"f\" \"firefox\" '(\"firefox\" \"http://www.baidu.com\"))"
    ""
    ))

(defun my-temp-setting ()
  "Create configure/temp-setting.el"
  (interactive)
  (let ((path  (concat zzemacs-path "/configure/" zz-dev-set-file)))
    (my-create-file path my-temp-setting)
    (message "create %s successful." path)
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


(defun newline-indents ()
  "Bind Return to `newline-and-indent' in the local keymap."
  (local-set-key "\C-m" 'newline-and-indent))

;; Tell Emacs to use the function above in certain editing modes.
(dolist (hook
          (list
           'lisp-mode-hook
           'emacs-lisp-mode-hook
           'lisp-interaction-mode-hook
           'scheme-mode-hook
           'c-mode-hook
           'c++-mode-hook
           'java-mode-hook
           'perl-mode-hook
           'python-mode-hook
           'php-mode-hook))
  (add-hook hook (function newline-indents)))

;;;cedet version flag t for inside
(defvar use-cedet-inside-flag (if (< emacs-major-version 24)
                                  nil
                                  t)
  "cedet using flag, t for use buildin, nil for office")
(if use-cedet-inside-flag
    (progn
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

      (global-ede-mode t)
      (semantic-mode t)
      )
    (progn
      ;; Disable cedet inside emacs
      (if-ms-windows
       (progn
         (setq load-path (remove (format "%s/lisp/cedet" (getenv "EMACS_DIR")) load-path)))
       (progn
         (setq load-path (remove "/usr/share/emacs/cedet" load-path))
         (setq load-path (remove (format "/usr/share/emacs/%s.%s/lisp/cedet"
                                         emacs-major-version emacs-minor-version)
                                 load-path))))
      
      (zz-load-path "site-lisp/cedet/common")
      
      ;; Load CEDET.
      ;; See cedet/common/cedet.info for configuration details.
      ;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
      ;; CEDET component (including EIEIO) gets activated by another 
      ;; package (Gnus, auth-source, ...).
      (require 'cedet)
      (global-ede-mode t)      
      (semantic-load-enable-minimum-features)
      (semantic-load-enable-code-helpers)

      ;; Enable source code folding
      (when window-system
          (global-semantic-tag-folding-mode 1))
      ))

;; 1:list methold in current buffer
;; 2:switch buffer in h & cpp file
(require 'eassist)
(define-key eassist-mode-map (kbd "TAB") 'eassist-jump-to-method)
(define-key eassist-mode-map (kbd "C-b") 'eassist-backspace-pressed)
(define-key eassist-mode-map (kbd "C-q") 'eassist-escape)

;; Semantic DataBase
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))

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
              (lambda ()
                (when (eq (current-buffer) speedbar-buffer)
                  (setq speedbar-frame nil
                        dframe-attached-frame nil
                        speedbar-buffer nil)
                  (speedbar-set-timer nil)))))
  (set-window-buffer (selected-window)
                     (get-buffer his-speedbar-buffer-name)))


;;;company-mode <f4+tab> to open complete menu
(defvar use-company-mode nil "using company mode flag")
(when use-company-mode
  (when-ms-windows 
   (zz-load-path "site-lisp/company-mode")
   (require 'company)
   (setq company-idle-delay 0.2)
   (setq company-minimum-prefix-length 1)
   (setq company-show-numbers t)
   (define-key company-active-map [return]    nil)
   (define-key company-active-map (kbd "RET") nil)
   (define-key company-active-map [tab]       'company-complete-selection)
   (define-key company-active-map (kbd "TAB") 'company-complete-selection)
   (dolist (hook (list
                  ;;'emacs-lisp-mode-hook
                  ;;'lisp-mode-hook
                  ;;'lisp-interaction-mode-hook
                  ;;'scheme-mode-hook
                  ;;'c-mode-hook
                  ;;'c++-mode-hook
                  ;;'java-mode-hook
                  ;;'perl-mode-hook
                  ;;'python-mode-hook
                  ;;'asm-mode-hook
                  'shell-mode-hook
                  ))
     (add-hook hook 'company-mode))))

;; auto complete
(zz-load-path "site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat zzemacs-path "/site-lisp/auto-complete/dict"))
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
(yas/load-directory (concat zzemacs-path "/site-lisp/yasnippet/snippets"))

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

;;load the etags-select.el source code
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
  (run-command-sort (gen-ctags-cmd dir-name)))

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
  (format "%s %s -type f \\( %s \\) -print | etags -"
          find-program dir-name (gen-find-parts my-find-regex)))

(defun create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (run-command-sort (gen-etags-cmd dir-name)))

;;make cscope
; #!/bin/bash  
; find -type f | egrep "\.[hc]$|hh$|cc$|[hc]pp$|[hc]xx$|[hc]\+\+$">cscope.files
; cscope -bq -i ./csope.files
(defun gen-cscope-cmd (dir-name)
  (concat
   (format "%s %s -type f \\( %s \\) -print > %s/cscope.files;"
           find-program dir-name (gen-find-parts my-find-regex)  default-directory)
   (format "cscope -b -R -q -i %s/cscope.files" default-directory)
   ))

(defun create-cscope (dir-name)
  "Create cscope file."
  (interactive "DDirectory: ")
  (run-command-sort (gen-cscope-cmd dir-name)))

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

;;javascript mode
(require 'js2-mode)

;;xmsi-mode for inputting math (Unicode) symbols.
(require 'xmsi-math-symbols-input)

;;for cdb.exe debug on windows
;;Use kd -k <connection string> instead of cdb <your program> e.g. M-x cdb kd -k com:port=com1
;;http://www.microsoft.com/whdc/devtools/debugging/default.mspx
;;A word for WinDbg
;;http://mtaulty.com/communityserver/blogs/mike_taultys_blog/archive/2004/08/03/4656.aspx
;;http://www.windbg.org/
;;http://www.windbg.info/
;;command line: "windbg.exe" -y "Debug;for-windbg\Debug" -srcpath "for-windbg" Debug\for-windbg.exe args
;;start folder: D:\work\for-windbg
;;Symbol Server (Microsoft): srv*c:\mss*http://msdl.microsoft.com/download/symbols
(when-ms-windows  
 (load-library "cdb-gud.el")
 (defun my-cdb-mode-hook ()
   (gud-def cdb-bc  "bc * "  nil "Breakpoint clean all."))
 (add-hook 'cdb-mode-hook 'my-cdb-mode-hook))

;;cmake file support
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;;Add code review note
(defun add-code-review-note ()
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


(provide 'prog-setting)

;;; prog-setting.el ends here
