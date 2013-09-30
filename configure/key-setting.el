;;;; key-setting.el --- key config file
;;;

;;;;function key setting on console
;;;keymap:zz/esc-fn-map        f1-f2  + 1/= <===> esc + f1/f12
;;;keymap:zz/f1-fn-map         f1-f3  + 1/= <===> f1  + f1/f12
;;;keymap:zz/f4-fn-map         f1-f4  + 1/= <===> f4  + f1/f12

;;;keymap:zz/ctr-x-fn-map      f1-f5  + 1/= <===> ctrl-x + f1/f12
;;;keymap:zz/ctr-c-fn-map      f1-f6  + 1/= <===> ctrl-c + f1/f12

;;;keymap:zz/ctrl-map          f1-f7  + 1/= <===> control + 1/=
;;;keymap:zz/alt-map           f1-f8  + 1/= <===> alt     + 1/=

;;;keymap:zz/fn-map            f1-f9  + 1/= <===> f1/f12
;;;keymap:zz/shift-fn-map      f1-f10 + 1/= <===> shift   + f1/f12
;;;keymap:zz/ctrl-fn-map       f1-f11 + 1/= <===> control + f1/f12
;;;keymap:zz/alt-fn-map        f1-f12 + 1/= <===> alt     + f1/f12

;;f4/esc-f4 key map 
(defvar f4-map (make-sparse-keymap) "f4 map for self functions.")
(define-key global-map [f4] f4-map)
(define-key help-map   "4"  f4-map)

;;f4-esc key map 
(defvar f4-esc-map (make-sparse-keymap) "f4-escape for extend functions.")
(define-key f4-map [escape] f4-esc-map)

;;f4-quote key map for help using
(defvar f4-backquote-map (make-sparse-keymap) "f4-backquote for self help function.")
(define-key f4-map (kbd "`") f4-backquote-map)

;;f4-e key map 
(defvar f4-e-map (make-sparse-keymap) "f4-e for execute functions.")
(define-key f4-map "e" f4-e-map)

;;f4-p key map 
(defvar f4-p-map (make-sparse-keymap) "f4-p for execute functions, can define in temp-setting.el.")
(define-key f4-map "p" f4-p-map)

;;f1-esc key map for help using
(defvar f1-esc-map (make-sparse-keymap) "f1-escape for self help functions.")
(define-key help-map [escape] f1-esc-map)

;;f1-quote key map for help using
(defvar f1-backquote-map (make-sparse-keymap) "f1-backquote for self help function.")
(define-key help-map (kbd "`") f1-backquote-map)

(when-ms-windows
 (setq w32-pass-rwindow-to-system nil)
 (setq w32-pass-lwindow-to-system nil)
 (setq w32-rwindow-modifier 'hyper)
 (setq w32-lwindow-modifier 'super))

;;multi key setting
(defun apply-keys-to-map (map key-pairs)
  "apply multi key defines"
  (let ((i 0))
    (while (< i (length key-pairs))
      (let ((key (nth i key-pairs))
            (fn (nth (1+ i) key-pairs)))
        (when fn
          (define-key map key fn)))
      (setq i (+ i 2)))))

;;f1-f7 => C-1/+
(defvar zz/ctrl-map  (make-sparse-keymap))
(define-key help-map (kbd "<f7>") zz/ctrl-map)

(apply-keys-to-map
 zz/ctrl-map
 (list 
  "`" 'imenu
  "1" 'delete-window
  "2" 'delete-frame
  "3" 'tabbar-backward-group
  "4" 'delete-frame
  "5" 'gud-until
  "6" 'gud-remove
  "7" 'gud-finish
  "8" 'gud-jump
  "9" 'gud-pstar
  "0" 'other-frame
  "-" 'undo
  "=" 'redo
  "," 'winner-undo
  "." 'winner-redo

  [(tab)] "\C-q\t"
  ))
 
;;f1-f8 => M-1/+
(defvar zz/alt-map (make-sparse-keymap))
(define-key help-map (kbd "<f8>") zz/alt-map)

(apply-keys-to-map
 zz/alt-map
 (list
  "1"  'delete-other-windows
  "2"  'delete-other-frames
  "3"  'tabbar-forward-group
  "4"  'kill-this-buffer
  "5"  (if-ms-windows 'gud-cont 'gud-go)
  
  "6"  'gud-break
  "7"  'gud-next
  "8"  'gud-step
  "9"  'gud-print
  "0"  'other-window
  
  "#"  'query-replace-regexp
  "]"  'tabbar-forward-tab
  "["  'tabbar-backward-tab
  
  "h"  'windmove-left
  "j"  'windmove-down
  "k"  'windmove-up
  "l"  'windmove-right
  ))

;;f1-f9 => f1/f12
(defvar zz/fn-map (make-sparse-keymap))
(define-key help-map [f9]  zz/fn-map)

;;f1-f10 => S-f1/f12
(defvar zz/shift-fn-map (make-sparse-keymap))
(define-key help-map [f10] zz/shift-fn-map)

;;f1-f11 => C-f1/f12
(defvar zz/ctrl-fn-map (make-sparse-keymap))
(define-key help-map [f11] zz/ctrl-fn-map)

;;f1-f12 => M-f1/f12
(defvar zz/alt-fn-map (make-sparse-keymap))
(define-key help-map [f12] zz/alt-fn-map)

;;f1-f5 => ctrl-x f1/f12
(defvar zz/ctrl-x-fn-map (make-sparse-keymap))
(define-key help-map (kbd "<f5>") zz/ctrl-x-fn-map)

;;f1-f5 => ctrl-c f1/f12
(defvar zz/ctrl-c-fn-map (make-sparse-keymap))
(define-key help-map (kbd "<f6>") zz/ctrl-c-fn-map)

;;group define fn key
(defun define-fn-key (fn-name &optional fn s-fn c-fn m-fn ctrl-x-fn ctrl-c-fn)
  (when fn        (define-key zz/fn-map        fn-name fn))
  (when s-fn      (define-key zz/shift-fn-map  fn-name s-fn))
  (when c-fn      (define-key zz/ctrl-fn-map   fn-name c-fn))
  (when m-fn      (define-key zz/alt-fn-map    fn-name m-fn))
  (when ctrl-x-fn (define-key zz/ctrl-x-fn-map fn-name ctrl-x-fn))
  (when ctrl-c-fn (define-key zz/ctrl-c-fn-map fn-name ctrl-c-fn)))

(define-fn-key "1"  ;;==> f1
    nil ;;[f1]
    nil ;;[S-f1]
    nil ;;[C-f1]
    nil ;;[M-f1]
    )

;;key for f1-backquote-map
(apply-keys-to-map
 f1-backquote-map
 (list
  (kbd "h") 'common-lisp-hyperspec
  (kbd "i") 'zz-info-open-file
  ))

;;execute start-process key
(apply-keys-to-map
 f4-e-map
 (list
  "1" (when-ms-windows (execute-set-key
                        "vs-x86-prompt"
                        '("cmd" "/c" "start" "vcvarsall" "x86")))
  "2" (when-ms-windows (execute-set-key
                        "vs-x64-prompt"
                        '("cmd" "/c" "start" "vcvarsall" "x64")))
  "f" (execute-set-key "firefox"  '("firefox" "http://www.baidu.com"))
  "b" (execute-set-key "bcompare" '("bcompare"))
  ))

;;switch to shells
(apply-keys-to-map
 f4-map
 (list  
  (kbd "<f4>")  'kill-this-buffer
  
  (kbd "<f9>")  (lambda () (interactive) (start-shell "*shell-f9*"))
  (kbd "<f10>") (lambda () (interactive) (start-shell "*shell-f10*"))
  (kbd "<f11>") (lambda () (interactive) (start-shell "*shell-f11*"))
  (kbd "<f12>") (lambda () (interactive) (start-shell "*shell-f12*"))
  
  (kbd "=")     'er/expand-region
  (kbd "-")     (if-emacs24-3 'smartparens-mode)
  
  (kbd "C-1")   'my-utf-8
  (kbd "C-h")   'sourcepair-jump-to-headerfile
  (kbd "C-l")   'command-history

  (kbd "C-d")   (if-ms-windows
                 (execute-set-key
                  "explorer"
                  (list "explorer"
                        (my-trans-path-sep
                         default-directory "/" "\\")))
                 'open-with-nautilus)  
  (kbd "C-t")   (unless-ms-windows 'open-with-terminal)
  ))

(define-fn-key "1"                   ;;==> f1
    nil                              ;;[f1]
    'planner-create-task-from-buffer ;;[S-f1]
    'session-save                    ;;[C-f1]
    'session-restore                 ;;[M-f1]
    nil                              ;;C-x f1
    nil                              ;;C-c f1
    )

(define-fn-key "2"                   ;;==> f2
    'bc-next                         ;;[f2]
    'bc-previous                     ;;[S-f2]
    'bc-set                          ;;[C-f2]
    'bc-list                         ;;[M-f2]
    nil                              ;;C-x f2
    nil                              ;;C-c f2
    )

(define-fn-key "3"                   ;;==> f3
    'my-last-buffer-go               ;;[f3]
    'list-bookmarks                  ;;[S-f3]
    'bc-local-next                   ;;[C-f3]
    'bc-local-previous               ;;[M-f3]
    'my-occur                        ;;C-x f3
    'my-woman-at-point               ;;C-c f3
    )

(define-fn-key "4"                   ;;==> f4
    (lookup-key zz/alt-map "4")      ;;[f4]
    'undo-kill-buffer                ;;[S-f4]
    'highlight-symbol-next           ;;[C-f4]
    'highlight-symbol-prev           ;;[M-f4]
    nil                              ;;C-x f4
    nil                              ;;C-c f4
    )

(define-fn-key "5"                   ;;==> f5
    'speedbar-get-focus              ;;[f5]
    'sr-speedbar-toggle              ;;[S-f5]
    'line-to-top-of-window           ;;[C-f5]
    'etags-stack-show                ;;[M-f5]
    nil                              ;;C-x f5
    nil                              ;;C-c f5
    )


(define-fn-key "6"                   ;;==> f6
    (if-ms-windows 'multi-shell-new  'get-term) ;;[f6]
    (if-ms-windows
     'multi-shell-current-directory
     'multi-term-dedicated-toggle)   ;;[S-f6]
    (if-ms-windows 'multi-shell-next 'multi-term-next) ;;[C-f6]
    (if-ms-windows 'multi-shell-prev 'multi-term-prev) ;;[M-f6]    
    (if-ms-windows 'switch-to-shell  'switch-to-term)  ;;C-x f6
    (unless-ms-windows 'switch-term-and-text)          ;;C-c f6
    )

(define-fn-key "7"                   ;;==> f7
    'compile                         ;;[f7]
    'switch-to-compilation           ;;[S-f7]
    'next-error                      ;;[C-f7]
    'previous-error                  ;;[M-f7]
    nil                              ;;C-x f7
    nil                              ;;C-c f7
    )

(define-fn-key "8"                   ;;==> f8
    'gdb                             ;;[f8]
    'gud-kill                        ;;[S-f8]
    'gdb-restore-windows             ;;[C-f8]
    'gdb-many-windows                ;;[M-f8]
    'gdb-use-separate-io             ;;C-x f8
    'gud-tooltip-mode                ;;C-c f8
    )

(define-fn-key "9"                   ;;==> f9
    (lambda () (interactive) (start-shell "*shell*")) ;;[f9]
    'multi-shell-new                 ;;[S-f9]
    'switch-to-scratch               ;;[C-f9]
    'popup-term                      ;;[M-f9]
    'switch-to-shell                 ;;C-x f9
    'eshell                          ;;C-c f9
    )

(define-fn-key "0"                   ;;==> f10
    nil                              ;;[f10]
    'tool-bar-mode                   ;;[S-f10]
    'my-toggle-maxframe              ;;[C-f10]
    'my-toggle-fullscreen            ;;[M-f10]
    'scroll-bar-mode                 ;;C-x f10
    'tabbar-mode                     ;;C-c f10
    )

(define-fn-key "-"                   ;;==> f11
    'linum-mode                      ;;[f11]
    'fci-mode                        ;;[S-f11]
    'hl-line-mode                    ;;[C-f11]
    'blank-mode                      ;;[M-f11]
    nil                              ;;C-x f11
    nil                              ;;C-c f11
    )

(define-fn-key "="                   ;;==> f12
    'find-grep                       ;;[f12]
    'rgrep                           ;;[S-f12]
    'find-name-dired                 ;;[C-f12]
    'my-c-rgrep                      ;;[M-f12]
    'my-unicad-switch                ;;(kbd "C-x <f12>")
    'my-os-file-switch               ;;(kbd "C-c <f12>")
    )

;;apply fn-key setting
(apply-keys-to-map
 global-map
 (list
  ;;for info
  [S-f1]    'planner-create-task-from-buffer
  [C-f1]    'session-save
  [M-f1]    'session-restore
  
  [f2]      'bc-next
  [S-f2]    'bc-previous
  [C-f2]    'bc-set
  [M-f2]    'bc-list
  
  [f3]      'my-last-buffer-go
  [S-f3]    'list-bookmarks
  [C-f3]    'bc-local-next
  [M-f3]    'bc-local-previous
  (kbd "C-x <f3>")  'my-occur
  (kbd "C-c <f3>")  'my-woman-at-point

  
  [S-f4]    'undo-kill-buffer
  [C-f4]    'highlight-symbol-next
  [M-f4]    'highlight-symbol-prev
  (kbd "C-x <f4>")  'recentf-open-files
  (kbd "C-c <f4>")  'recentf-open-files-compl

  
  [f5]      'speedbar-get-focus
  [S-f5]    'sr-speedbar-toggle
  [C-f5]    'line-to-top-of-window
  [M-f5]    'etags-stack-show

  [f6]      (if-ms-windows 'multi-shell-new  'get-term)
  [S-f6]    (if-ms-windows
             'multi-shell-current-directory
             'multi-term-dedicated-toggle)
  [C-f6]    (if-ms-windows 'multi-shell-next 'multi-term-next)
  [M-f6]    (if-ms-windows 'multi-shell-prev 'multi-term-prev)
  (kbd "C-x <f6>")  (if-ms-windows 'switch-to-shell 'switch-to-term)
  (kbd "C-c <f6>")  (unless-ms-windows 'switch-term-and-text)
  
  [f7]      'compile
  [S-f7]    'switch-to-compilation
  [C-f7]    'next-error
  [M-f7]    'previous-error
  
  [f8]      'gdb
  [S-f8]    'gud-kill
  [C-f8]    'gdb-restore-windows
  [M-f8]    'gdb-many-windows
  (kbd "C-x <f8>")  'gdb-use-separate-io
  (kbd "C-c <f8>")  'gud-tooltip-mode
  
  [f9]      (lambda () (interactive) (start-shell "*shell*"))
  [S-f9]    'multi-shell-new
  [C-f9]    'switch-to-scratch
  [M-f9]    'popup-term
  (kbd "C-x <f9>")  (lookup-key zz/ctrl-x-fn-map "9")
  (kbd "C-c <f9>")  'eshell
  
  [S-f10]   'tool-bar-mode
  [C-f10]   'my-toggle-maxframe
  [M-f10]   'my-toggle-fullscreen
  (kbd "C-x <f10>") 'scroll-bar-mode
  (kbd "C-c <f10>") 'tabbar-mode
  
  [f11]     'linum-mode
  [S-f11]   'fci-mode
  [C-f11]   'hl-line-mode
  [M-f11]   'blank-mode
  
  [f12]     'find-grep
  [S-f12]   'rgrep
  [C-f12]   'find-name-dired
  [M-f12]   'my-c-rgrep
  (kbd "C-x <f12>") 'my-unicad-switch
  (kbd "C-c <f12>") 'my-os-file-switch
  ))

;;number 0-1/-/=
(apply-keys-to-map
 global-map
 (list
  (kbd "M-1") (lookup-key zz/alt-map "1")
  (kbd "M-2") (lookup-key zz/alt-map "2")
  (kbd "M-3") (lookup-key zz/alt-map "3")
  (kbd "M-4") (lookup-key zz/alt-map "4")
  ;;gud control setting
  (kbd "M-5") (lookup-key zz/alt-map "5")
  (kbd "M-6") (lookup-key zz/alt-map "6")
  (kbd "M-7") (lookup-key zz/alt-map "7")
  (kbd "M-8") (lookup-key zz/alt-map "8")
  (kbd "M-9") (lookup-key zz/alt-map "9")
  (kbd "M-0") (lookup-key zz/alt-map "0")

  (kbd "C-`") (lookup-key zz/ctrl-map "`")
  (kbd "C-1") (lookup-key zz/ctrl-map "1")
  (kbd "C-2") (lookup-key zz/ctrl-map "2")
  (kbd "C-3") (lookup-key zz/ctrl-map "3")
  (kbd "C-4") (lookup-key zz/ctrl-map "4")
  ;;gud control setting
  (kbd "C-5") (lookup-key zz/ctrl-map "5")
  (kbd "C-6") (lookup-key zz/ctrl-map "6")
  (kbd "C-7") (lookup-key zz/ctrl-map "7")
  (kbd "C-8") (lookup-key zz/ctrl-map "8")
  (kbd "C-9") (lookup-key zz/ctrl-map "9")
  (kbd "C-0") (lookup-key zz/ctrl-map "0")
  ))

;;f1 1,2,3,4 for highlight-symbol
(apply-keys-to-map
 help-map
 (list
  (kbd "1") 'highlight-symbol-at-point
  (kbd "2") 'highlight-symbol-remove-all
  (kbd "3") 'highlight-symbol-query-replace
  ;;"4" -> f4-map
  
  ;;gdb frame show setting
  (kbd "5") 'gdb-frame-stack-buffer
  (kbd "6") 'gdb-frame-breakpoints-buffer
  (kbd "7") 'gdb-frame-assembler-buffer
  (kbd "8") 'gdb-frame-memory-buffer
  (kbd "9") 'gdb-frame-locals-buffer
  (kbd "0") 'gdb-frame-gdb-buffer
  (kbd "-") 'gud-up
  (kbd "=") 'gud-down
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x C-b") 'ibuffer
  (kbd "C-x C-j") 'dired-jump
  (kbd "C-c w")   'compare-windows
  (kbd "M-#")     (lookup-key zz/alt-map "#")

  (kbd "%")       'match-paren

  [backspace]     'delete-backward-char
  [delete]        'delete-char

  [C-wheel-up]    (when-ms-windows 'text-scale-increase)
  [C-wheel-down]  (when-ms-windows 'text-scale-decrease)
  [C-mouse-4]     (unless-ms-windows 'text-scale-increase)
  [C-mouse-5]     (unless-ms-windows 'text-scale-decrease)

  ;;Control tab quotes a tab => "\C-q\t"
  [(control tab)] (lookup-key zz/ctrl-map [(tab)])

  ;;tabbar switch group
  (kbd "M-]")     (lookup-key zz/alt-map "]")
  (kbd "M-[")     (lookup-key zz/alt-map "[")

  (kbd "C-c h")   'helm-mini

  ;;undo/redo
  (kbd "C--")     (lookup-key zz/ctrl-map "-")
  (kbd "C-=")     (lookup-key zz/ctrl-map "=")

  ;;winner restore
  (kbd "C-,")     (lookup-key zz/ctrl-map ",")
  (kbd "C-.")     (lookup-key zz/ctrl-map ".")

  ;;quick move other windows
  [M-up]          (lookup-key zz/alt-map "k")
  [M-down]        (lookup-key zz/alt-map "j")
  [M-right]       (lookup-key zz/alt-map "l")
  [M-left]        (lookup-key zz/alt-map "h")

  ;;window size change
  [S-up]         'enlarge-window
  [S-down]       'shrink-window
  [S-right]      'enlarge-window-horizontally
  [S-left]       'shrink-window-horizontally

  ;;set apps do M+x
  [apps]         'execute-extended-command
  ))

(provide 'key-setting)

;;; key-setting.el ends here
