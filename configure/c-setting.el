;;;; c-setting.el --- c/c++ config file
;;

;;http://cc-mode.sourceforge.net/
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

;;https://github.com/dkogan/xcscope.el
(require 'xcscope)
(setq cscope-option-use-inverted-index t
      cscope-close-window-after-select t)

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

;;;auto-complete-clang
;;sudo apt install clang
(defun my:get-include-dirs ()
  (let* ((command-result (shell-command-to-string "echo \"\" | g++ -v -x c++ -E -"))
         (start-string "#include <...> search starts here:\n")
         (end-string "End of search list.\n")
         (start-pos (string-match start-string command-result))
         (end-pos (string-match end-string command-result))
         (include-string (substring command-result (+ start-pos (length start-string)) end-pos)))
    (split-string include-string)))

;;sudo apt install llvm-dev libclang-dev
;;sudo yum install llvm-devel clang-devel
;;sudo pacman -S clang llvm
;;(executable-find "clang++")
;;https://www.hiroom2.com/2016/10/31/emacs-auto-complete-clang-async-package
(defvar my:clang-async-p  t "flag for use clang async")
(defun my:ac-clang-init ()
  (require 'auto-complete-clang)
  (setq ac-clang-complete-executable "clang++")
  (setq ac-clang-flags
        (mapcar (lambda (item)
                  (concat "-I" item))
                (my:get-include-dirs)))
  (add-to-list 'ac-sources 'ac-source-clang))

(defun my:ac-clang-async-init ()
  (require 'auto-complete-clang-async)
  (setq ac-clang-complete-executable "clang-complete")
  (setq ac-clang-flags
        (mapcar (lambda (item)
                  (concat "-I" item))
                (my:get-include-dirs)))
  (add-to-list 'ac-sources 'ac-source-clang-async)
  (ac-clang-launch-completion-process))

(if my:clang-async-p
    (add-hook 'c-mode-common-hook 'my:ac-clang-async-init)
    (add-hook 'c-mode-common-hook 'my:ac-clang-init))

;;auto complete c header
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (mapc
   (lambda (inc)
     (add-to-list 'achead:include-directories inc))
   (my:get-include-dirs)))

(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

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
(defun my:c-mode-common-hook()
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

(add-hook 'c-mode-common-hook 'my:c-mode-common-hook)

;; my c setting
(defun my:c-mode-hook()
  (c-set-style "ffmpeg"))

(add-hook 'c-mode-hook 'my:c-mode-hook)

;; my c++ setting
(defun my:c++-mode-hook()
  (c-set-style "stroustrup"))

(add-hook 'c++-mode-hook 'my:c++-mode-hook)

;; switch c and c++ mode
(defun my:c-c++-toggle ()
  "toggles between c-mode and c++-mode"
  (interactive)
  (cond ((string= major-mode "c-mode")
         (c++-mode))
        ((string= major-mode "c++-mode")
         (c-mode))))


(provide 'c-setting)

;;; c-setting.el ends here
