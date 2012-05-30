;;;; prog-setting.el --- program common file
;;
;; holding
(load-library "hideshow")
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
(setq  my-c/c++-file-regex
      (concat "-name \"*.[hcHC]\" -print -or "
              "-name \"*.[hc]pp\" -print -or "
              "-name \"*.[hc]++\" -print -or "
              "-name \"*.[hc]xx\" -print "
              ))

(defun gen-etags-cmd (dir-name)
  (format "find %s -type f %s | etags -"
          dir-name  my-c/c++-file-regex))

(defun create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (async-shell-command (gen-etags-cmd dir-name)))

;;make cscope
; #!/bin/bash  
; find -type f | grep -E "\.(c|cc|cpp|cxx|c\+\+|h|h\+\+|hh|hpp|hxx)$" >cscope.files
; cscope -bq -i ./csope.files
(defun gen-cscope-cmd (dir-name)
  (concat
   (format "find %s -type f %s > %s/cscope.files;"
           dir-name  my-c/c++-file-regex  dir-name)
   (format "cscope -bkq -i  %s/cscope.files" dir-name)
   ))

(defun create-cscope (dir-name)
  "Create cscope file."
  (interactive "DDirectory: ")
  (async-shell-command (gen-cscope-cmd dir-name)))

;; SET TAGS PATH
;(setq tags-table-list '("~/work/TAGS"
;                        ;;"~/TAGS"
;                        ))

(setq auto-mode-alist
   (append
    (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
    (list (cons "\\.[cC][mM][dD]$" 'bat-mode))
    ;; For DOS init files
    ;(list (cons "CONFIG\\."   'bat-mode))
    ;(list (cons "AUTOEXEC\\." 'bat-mode))
    auto-mode-alist))

(autoload 'bat-mode "bat-mode"
    "DOS and WIndows BAT files" t)

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
   (cons "\\.sq$" 'sql-mode))
  auto-mode-alist))

(add-hook 'sql-mode-hook 'font-lock-mode)

;;gud setting
(require 'gdb-ui)
(defun gdb-or-gud-go ()
  "If gdb isn't running; run gdb, else call gud-go."
  (interactive)
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer)
           (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba)))
      (gud-call (if gdb-active-process "continue" "run") "")
    (gdb (gud-query-cmdline 'gdb))))

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
  (kill-process (get-buffer-process gud-comint-buffer)))

;;SVN Support
;(require 'psvn)
;(require 'vc-svn)

;;Git Support
(zz-load-path "site-lisp/git-emacs")
(require 'git-emacs)

;;rgrep for c/c++
(setq my-c-file-regex "*.[hc]")
(defun my-c-rgrep (term &optional dir)
  (interactive (list (completing-read "Search Term: " nil nil nil (thing-at-point 'word)))) 
  (grep-compute-defaults) 
  (let* ((dir (read-directory-name "Base directory: " nil default-directory t)))
    (rgrep term my-c-file-regex dir)))

(provide 'prog-setting)

;;; c-setting.el ends here
