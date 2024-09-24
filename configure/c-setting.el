;;;; c-setting.el --- c/c++ config file
;;

;;https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
;;http://cc-mode.sourceforge.net/
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

;;gtags global
;$ apt install global
;$ gtags -v
;(require 'gtags)

;;rtags
;;https://github.com/Andersbakken/rtags
;$ rdm &
;$ cd /path/to/project/root
;$ cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
;$ rc -J .
;(zz/load-path "site-lisp/rtags")
;(require 'rtags)

;;holding #if
(load-library "hideif")

;;find source pair
;;switch between header and source files
(require 'cff)

;;https://en.wikipedia.org/wiki/Indentation_style
;;https://pages.cs.wisc.edu/~driscoll/software/emacs/indent-styles.html
;;add linux kernel style
(c-add-style "kernel"
             '( "linux"
               (c-basic-offset . 8)
               (indent-tabs-mode . t)  ;;kernel use tab to indent
               (tab-width . 8)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist
                (brace-list-open)
                (brace-entry-open)
                (substatement-open after)
                (block-close . c-snug-do-while)
                (arglist-cont-nonempty))
               (c-cleanup-list brace-else-brace)
               (c-offsets-alist
                (statement-block-intro . +)
                (knr-argdecl-intro . 0)
                (substatement-open . 0)
                (substatement-label . 0)
                (label . 0)
                (statement-cont . +))
               ))

;;c/c++ code style
(c-add-style "zach"
             '( "k&r"
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (show-trailing-whitespace . t)
               (tab-width . 2)
               (c-hanging-braces-alist
                (brace-list-open)
                (brace-entry-open)
                (substatement-open after)
                (block-close . c-snug-do-while)
                (arglist-cont-nonempty))
               (c-cleanup-list brace-else-brace)
               (c-offsets-alist
                (statement-block-intro . +)
                (knr-argdecl-intro . 0)
                (substatement-open . 0)
                (substatement-label . 0)
                (label . 0)
                (statement-cont . +))
               ))

(c-add-style "ffmpeg"
             '("k&r"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)
               (show-trailing-whitespace . t)
               (c-offsets-alist
                (statement-cont . (c-lineup-assignments +)))
               ))

;;c common setting hook
(defkeys-map c-mode-base-map
  ((kbd "TAB")     'company-indent-or-complete-common)
  ((kbd "C-c M-/") 'zz/company-ctags)
  ((kbd "M-o")     'cff-find-other-file)
  ((kbd "M-m")     'eassist-list-methods))
(defun zz/c-mode-common-hook()
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  ;;process settings
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (abbrev-mode t)
  (hide-ifdef-mode t))
(add-hook 'c-mode-common-hook 'zz/c-mode-common-hook)

;;c setting hook
(defun zz/c-mode-hook()
  (c-set-style "zach"))
(add-hook 'c-mode-hook 'zz/c-mode-hook)

;;c++ setting hook
(defun zz/c++-mode-hook()
  (c-set-style "zach"))
(add-hook 'c++-mode-hook 'zz/c++-mode-hook)

;;switch c and c++ mode
(defun zz/c-c++-toggle ()
  "toggles between c-mode and c++-mode"
  (interactive)
  (cond ((string= major-mode "c-mode")
         (c++-mode))
        ((string= major-mode "c++-mode")
         (c-mode))))

;;c headers
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;;lsp-mode and eglot-mode support
;;https://clangd.llvm.org/installation.html
;;https://ddavis.io/blog/eglot-cpp-ide/
;;https://joaotavora.github.io/eglot/
;;sudo apt install bear && bear cmake && bear make
;;sudo apt install clangd ccls
(defvar zz/c-lang-server "ccls-wrapper" "ccls-wrapper or clangd-wrapper")
(defun zz/c-eglot-enable ()
  "set variables and hook for eglot c/c++ IDE"
  (interactive)
  (setq company-backends
        (cons 'company-capf
              (remove 'company-capf company-backends)))
  (add-to-list 'eglot-server-programs
               `((c++-mode c-mode) ,zz/c-lang-server))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (message "Use eglot with %s for c/c++ mode"
           zz/c-lang-server))
(defun zz/c-eglot-disable ()
  "remove hook for c/c++"
  (interactive)
  (remove-hook 'c-mode-hook 'eglot-ensure)
  (remove-hook 'c++-mode-hook 'eglot-ensure))

;;https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
;;https://emacs-lsp.github.io/lsp-mode/page/lsp-clangd/
(defun zz/c-lsp-enable ()
  "set variables and hook for lsp c/c++ IDE"
  (interactive)
  (setq company-backends
        (cons 'company-capf
              (remove 'company-capf company-backends)))
  (require 'lsp-clangd)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection zz/c-lang-server)
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  (add-hook 'c-mode-hook 'lsp-deferred)
  (add-hook 'c++-mode-hook 'lsp-deferred)
  (message "Use lsp-mode with %s for c/c++ mode"
           zz/c-lang-server))
(defun zz/c-lsp-disable ()
  "remove hook for lsp c/c++"
  (interactive)
  (remove-hook 'c-mode-hook 'lsp-deferred)
  (remove-hook 'c++-mode-hook 'lsp-deferred))

(defvar zz/c-eglot-p t "t for eglot, nil for lsp-mode")
(if zz/c-eglot-p
  (zz/c-eglot-enable)
  (zz/c-lsp-enable))


(provide 'c-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; c-setting.el ends here
