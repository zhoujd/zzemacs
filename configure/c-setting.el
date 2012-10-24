;;;; c-setting.el --- c/c++ config file
;;

;;http://cc-mode.sourceforge.net/
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

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

;; 1:list methold in current buffer
;; 2:switch buffer in h & cpp file
(require 'eassist)
(require 'sourcepair)
(setq sourcepair-source-path    '( "." "../*" "../../*" ))
(setq sourcepair-header-path    '( "." "include" "../include" "../*" "../../*"))
(setq sourcepair-recurse-ignore '( "CVS"  "Debug" "Release" ))

;;sudo apt-get install clang
;(require 'auto-complete-clang)
;(setq ac-clang-flags  (list   
;                       "-I/usr/include"
;                       "-I/usr/local/include"
;                       ))
;
;(add-to-list 'ac-sources 'ac-source-clang)

;;;; Include settings
(if (not (or (eq window-system 'w32)
             (eq window-system 'win32)))
    (progn
      (require 'semantic/bovine/gcc)
      (require 'semantic/bovine/c)

      (defconst cedet-user-include-dirs
        (list ".." "../include" "../inc" "../common" "../public" "."
              "../.." "../../include" "../../inc" "../../common" "../../public"))

      (setq cedet-sys-include-dirs (list
                                    "/usr/include"
                                    "/usr/include/bits"
                                    "/usr/include/glib-2.0"
                                    "/usr/include/gnu"
                                    "/usr/include/gtk-2.0"
                                    "/usr/include/gtk-2.0/gdk-pixbuf"
                                    "/usr/include/gtk-2.0/gtk"
                                    "/usr/local/include"))

      (let ((include-dirs cedet-user-include-dirs))
        (setq include-dirs (append include-dirs cedet-sys-include-dirs))
        (mapc (lambda (dir)
                (semantic-add-system-include dir 'c++-mode)
                (semantic-add-system-include dir 'c-mode))
              include-dirs))

      (setq semantic-c-dependency-system-include-path "/usr/include/")))

;;; my c setting hook
(defun my-c-mode-common-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (add-to-list 'ac-sources 'ac-source-semantic)
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
  (local-set-key "\C-c." 'ac-complete-semantic)

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
