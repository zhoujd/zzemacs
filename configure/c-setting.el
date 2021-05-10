;;;; c-setting.el --- c/c++ config file
;;

;;http://cc-mode.sourceforge.net/
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

;;https://github.com/dkogan/xcscope.el
(require 'xcscope)
(setq cscope-option-use-inverted-index t)
(cscope-setup)

;;gtags global
;;$sudo apt-get install global
;;$gtags -v
;;start Emacs and execute gtags-mode function.
;(autoload 'gtags-mode "gtags" "" t)

;;holding #if
(load-library "hideif")

;;source pair
(require 'sourcepair)
(setq sourcepair-source-path    '( "." "../*" "../../*" ))
(setq sourcepair-header-path    '( "." "include" "../include" "../*" "../../*"))
(setq sourcepair-recurse-ignore '( "CVS"  "Debug" "Release" ))

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
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)
               (show-trailing-whitespace . t)
               (tab-width . 4)
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

;; my c setting hook
(defun zz:c-mode-common-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (defkeys-map c-mode-base-map
    ((kbd "M-o")     'eassist-switch-h-cpp)
    ((kbd "M-m")     'eassist-list-methods)
    ([(control tab)] 'ac-complete-clang))
  ;;process settings
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (abbrev-mode t)
  (hide-ifdef-mode t))

(add-hook 'c-mode-common-hook 'zz:c-mode-common-hook)

;; my c setting
(defun zz:c-mode-hook()
  (c-set-style "ffmpeg"))

(add-hook 'c-mode-hook 'zz:c-mode-hook)

;; my c++ setting
(defun zz:c++-mode-hook()
  (c-set-style "stroustrup"))

(add-hook 'c++-mode-hook 'zz:c++-mode-hook)

;; switch c and c++ mode
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

;; list methold in current buffer
;; switch buffer in h & cpp file
(require 'eassist)
(defkeys-map eassist-mode-map
  ((kbd "TAB") 'eassist-jump-to-method)
  ((kbd "C-b") 'eassist-backspace-pressed)
  ((kbd "C-q") 'eassist-escape))


(provide 'c-setting)

;;; c-setting.el ends here
