;;;; c-setting.el --- c/c++ config file
;;

;;http://cc-mode.sourceforge.net/
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

;;win32->http://sourceforge.net/project/showfiles.php?group_id=196604
(require 'xcscope)
(setq cscope-do-not-update-database t)

;(setq cscope-database-regexps
;      '(
;        ("/home/zhoujd/freebsd8/"       (t ("-q" "-d")) t)
;        ("/home/zhoujd/nginx-0.8.54/"   (t ("-q" "-d")) t)
;        ))

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

;;sudo apt-get install clang
;(require 'auto-complete-clang)

;;;; Include settings
(unless-ms-windows    
 (progn
   (require 'semantic/bovine/gcc)
   (defconst cedet-user-include-dirs (list
                                      "."
                                      ".."
                                      ))
   
   (setq cedet-sys-include-dirs (list
                                 "/usr/include"
                                 "/usr/local/include"
                                 ))
   
   (let ((include-dirs cedet-user-include-dirs))
     (setq include-dirs (append include-dirs cedet-sys-include-dirs))
     (mapc (lambda (dir)
             (semantic-add-system-include dir 'c++-mode)
             (semantic-add-system-include dir 'c-mode))
           include-dirs))
   
   (setq semantic-c-dependency-system-include-path "/usr/include/")))

;;add linux kernel style
(c-add-style "kernel-coding"
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
(c-add-style "my-coding-style"
             '( "k&r"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)
               (tab-width . 4)
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

(defvar kernel-keywords '("kernel" "driver")
  "Keywords which are used to indicate this file is kernel code.")

(add-hook 'c-mode-hook
          (lambda ()
            (let* ((filename (buffer-file-name))
                   (is-kernel-code nil))
              (if filename
                  (dolist (keyword kernel-keywords)
                          (if (string-match keyword filename)
                              (setq is-kernel-code t))))
              (if is-kernel-code
                  (c-set-style "kernel-coding")
                  (c-set-style "my-coding-style")))))

;;; my c setting hook
(defun my-c-mode-common-hook()
  ;; Semantic functions.
  (semantic-default-c-setup)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cR" 'semantic-symref)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-ia-show-summary)
  (local-set-key "\C-cl" 'semantic-ia-show-doc)
  (local-set-key "\C-cr" 'semantic-symref-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  ;(local-set-key "." 'semantic-complete-self-insert)
  ;(local-set-key ">" 'semantic-complete-self-insert)

  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)
  ;;process settings
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  ;;(imenu-add-to-menubar "Tags")
  ;;(gtags-mode t)
  (abbrev-mode t)
  (hide-ifdef-mode t))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;;my c++ setting
(defun my-c++-mode-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (c-set-style "stroustrup"))

;; c++-mode
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook   'my-c++-mode-hook)

(provide 'c-setting)

;;; c-setting.el ends here
