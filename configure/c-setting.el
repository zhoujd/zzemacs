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
;(zz:load-path "site-lisp/rtags")
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
  ((kbd "M-o")     'cff-find-other-file)
  ((kbd "M-m")     'eassist-list-methods))
(defun zz:c-mode-common-hook()
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  ;;process settings
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (abbrev-mode t)
  (hide-ifdef-mode t))
(add-hook 'c-mode-common-hook 'zz:c-mode-common-hook)

;;c setting hook
(defun zz:c-mode-hook()
  (c-set-style "zach"))
(add-hook 'c-mode-hook 'zz:c-mode-hook)

;;c++ setting hook
(defun zz:c++-mode-hook()
  (c-set-style "zach"))
(add-hook 'c++-mode-hook 'zz:c++-mode-hook)

;;switch c and c++ mode
(defun zz:c-c++-toggle ()
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
;;https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
;;https://emacs-lsp.github.io/lsp-mode/page/lsp-clangd/
;;https://clangd.llvm.org/installation.html
;;https://ddavis.io/blog/eglot-cpp-ide/
;;https://joaotavora.github.io/eglot/
;;sudo apt install bear && bear cmake && bear make
;;sudo apt install clangd
;;sudo apt install ccls
(defvar zz:c-lsp-eglot-p t "t for eglot, nil for lsp-mode")
(defvar zz:c-lang-server "clangd" "ccls or clangd")
(if zz:c-lsp-eglot-p
    (progn
      (add-to-list 'eglot-server-programs
                   '((c++-mode c-mode) zz:c-lang-server))
      (add-hook 'c-mode-hook 'eglot-ensure)
      (add-hook 'c++-mode-hook 'eglot-ensure))
    (progn
      (require 'lsp-clangd)
      (setq lsp-clients-clangd-args '("-j=4"
                                      "--background-index"
                                      "--limit-references=0"
                                      "--clang-tidy"
                                      "-log=error"))
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-tramp-connection zz:c-lang-server)
                        :major-modes '(c-mode c++-mode)
                        :remote? t
                        :server-id 'clangd-remote))
      (add-hook 'c-mode-hook 'lsp-deferred t)
      (add-hook 'c++-mode-hook 'lsp-deferred t)
      ))

(defvar zz:company-lsp-p nil "t for enable, nil for disable")
(when zz:company-lsp-p
  ;;use company-mode with local 'company-backends
  (defun zz:c-company-hook ()
    (set (make-local-variable 'company-backends)
         '(company-lsp)))
  (add-hook 'c-mode-hook 'zz:c-company-hook t)
  (add-hook 'c++-mode-hook 'zz:c-company-hook t))


(provide 'c-setting)

;;; c-setting.el ends here
