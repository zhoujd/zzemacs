;;;; complete-setting.el --- complete common file


(zz/load-path "site-lisp")

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
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(defun zz/expand-file-name ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially
                                            try-complete-file-name)))
    (call-interactively 'hippie-expand)))

(defun zz/indent-or-complete ()
   "complete if point is at end of a word, otherwise indent line"
   (interactive)
   (if (looking-at "\\>")
       (hippie-expand nil)
       (indent-for-tab-command)))

;;YASNIPPET
;;https://github.com/capitaomorte/yasnippet
(zz/load-path "site-lisp/yasnippet")
(zz/load-path "site-lisp/yasnippet-snippets")
(require 'yasnippet-snippets)
(yas-global-mode t)

;;code completion and snippets
;;https://robert.kra.hn/posts/rust-emacs-setup/
(defun zz/check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun zz/do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun zz/tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (zz/do-yas-expand)))
          (if (zz/check-expansion)
              (company-complete-common)
              (indent-for-tab-command)))))

(defun zz/company-yasnippet-or-completion ()
  (interactive)
  (or (zz/do-yas-expand)
      (company-complete-common)))

;;lsp-mode
;;https://emacs-lsp.github.io/lsp-mode/
;;https://systemcrafters.net/emacs-from-scratch/build-your-own-ide-with-lsp-mode/
;;https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(zz/load-path "site-lisp/lsp-mode")
(setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
(require 'lsp-mode)
(require 'lsp-lens)
(require 'lsp-modeline)
(require 'lsp-headerline)
(setq lsp-lens-enable nil
      lsp-diagnostic-package :none
      lsp-modeline-code-actions-enable nil
      lsp-modeline-diagnostics-enable nil
      lsp-enable-symbol-highlighting nil
      lsp-warn-no-matched-clients nil
      lsp-enable-on-type-formatting nil
      lsp-headerline-breadcrumb-enable nil)

(setq lsp-auto-configure t
      lsp-auto-guess-root t
      lsp-idle-delay 0.500
      lsp-session-file "~/.emacs.d/.cache/lsp-sessions")

;;lsp-mode with which-key
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
;;disable lsp info
(defun lsp--info (format &rest args)
  "Display lsp info message with FORMAT with ARGS."
  t)

;;https://clangd.llvm.org/installation.html
;;https://github.com/joaotavora/eglot
(zz/load-path "site-lisp/emacs-legcy")
(require 'eglot)
(setq eglot-events-buffer-size 0
      eglot-sync-connect 0
      eglot-ignored-server-capabilities '(:documentFormattingProvider
                                          :documentOnTypeFormattingProvider))
(add-to-list 'eglot-stay-out-of 'flymake)
(add-to-list 'eglot-stay-out-of 'imenu)
(defun eglot--message (format &rest args)
  "Message out with FORMAT with ARGS."
  t)


(provide 'complete-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; complete-setting.el ends here
