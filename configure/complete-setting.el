;;;; complete-setting.el --- complete common file


(zz-load-path "site-lisp")


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



(provide 'complete-setting)

;;; complete-setting.el ends here
