;;;; complete-setting.el --- complete common file


(zz:load-path "site-lisp")

;; Semantic DataBase
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))

;; hippie-try-expand settings
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs))

(defun zz:expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially
                                            try-complete-file-name)))
    (call-interactively 'hippie-expand)))

;;; https://www.emacswiki.org/emacs/InsertFileName
;;Key sequence	Inserted text
;;C-c C-i ~/.emacs RET	~/.emacs
;;C-u C-c C-i ~/.emacs RET	/home/user/.emacs
;;C-- C-c C-i ~/.emacs RET	.emacs
(defun zz:insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point."
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

;; bind it
(defkeys-map global-map
  ((kbd "C-c C-i") 'zz:insert-file-name)
  ((kbd "C-M-/")   'zz:expand-file-name-at-point))

(defun zz:indent-or-complete ()
   "complete if point is at end of a word, otherwise indent line"
   (interactive)
   (if (looking-at "\\>")
       (hippie-expand nil)
       (indent-for-tab-command)))

;;YASNIPPET
;;https://github.com/capitaomorte/yasnippet
(zz:load-path "site-lisp/yasnippet")
(zz:load-path "site-lisp/yasnippet-snippets")
(require 'yasnippet-snippets)
(yas-global-mode t)

;;lsp-mode
;;https://emacs-lsp.github.io/lsp-mode/
;;https://systemcrafters.net/emacs-from-scratch/build-your-own-ide-with-lsp-mode/
;;https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(zz:load-path "site-lisp/lsp-mode")
(setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
(require 'lsp-mode)
(require 'lsp-lens)
(require 'lsp-modeline)
(require 'lsp-headerline)
(setq lsp-lens-enable nil
      lsp-enable-symbol-highlighting nil
      lsp-headerline-breadcrumb-enable nil)

;;;;https://clangd.llvm.org/installation.html
;;https://github.com/joaotavora/eglot
(zz:load-path "site-lisp/emacs-legcy")
(require 'eglot)


(provide 'complete-setting)

;;; complete-setting.el ends here
