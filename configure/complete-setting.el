;;;; complete-setting.el --- complete common file


(zz-load-path "site-lisp")

;; cedet setting
(require 'cedet)
(require 'semantic/sb)

;; select which submodes we want to activate
(mapc (lambda (MODE) (add-to-list 'semantic-default-submodes MODE))
      '(global-semantic-idle-scheduler-mode
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

;;company-mode <ctrl+tab> to open complete menu
;(zz-load-path "site-lisp/company-mode")
;(require 'company)
;(global-company-mode t)
;(setq company-idle-delay nil) ;; nil for not auto popup
;(setq company-show-numbers t)
;(setq company-minimum-prefix-length 1)
;(define-key company-active-map [return]    nil)
;(define-key company-active-map (kbd "RET") nil)
;(define-key company-active-map [tab]       'company-complete-selection)
;(define-key company-active-map (kbd "TAB") 'company-complete-selection)
;(global-set-key [(control tab)] 'company-complete)

;; auto complete
(zz-load-path "site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat zzemacs-path "/site-lisp/auto-complete/dict"))
(ac-config-default)

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

;;YASNIPPET
;;https://github.com/capitaomorte/yasnippet
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
