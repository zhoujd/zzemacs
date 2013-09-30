;;;; fn-setting.el --- key function config file
;;;

;;;;function key setting on console
;;;keymap:zz/esc-fn-map        f1-f2  + 1/= <===> esc + f1/f12
;;;keymap:zz/f1-fn-map         f1-f3  + 1/= <===> f1  + f1/f12
;;;keymap:zz/f4-fn-map         f1-f4  + 1/= <===> f4  + f1/f12

;;;keymap:zz/ctr-x-fn-map      f1-f5  + 1/= <===> ctrl-x + f1/f12
;;;keymap:zz/ctr-c-fn-map      f1-f6  + 1/= <===> ctrl-c + f1/f12

;;;keymap:zz/fn-map            f1-f9  + 1/= <===> f1/f12
;;;keymap:zz/shift-fn-map      f1-f10 + 1/= <===> shift   + f1/f12
;;;keymap:zz/ctrl-fn-map       f1-f11 + 1/= <===> control + f1/f12
;;;keymap:zz/alt-fn-map        f1-f12 + 1/= <===> alt     + f1/f12

;;f1-f9 => f1/f12
(defvar zz/fn-map (make-sparse-keymap) "f1-f9 => f1/f12")
(define-key help-map [f9]  zz/fn-map)

;;f1-f10 => S-f1/f12
(defvar zz/shift-fn-map (make-sparse-keymap) "f1-f10 => S-f1/f12")
(define-key help-map [f10] zz/shift-fn-map)

;;f1-f11 => C-f1/f12
(defvar zz/ctrl-fn-map (make-sparse-keymap) "f1-f11 => C-f1/f12")
(define-key help-map [f11] zz/ctrl-fn-map)

;;f1-f12 => M-f1/f12
(defvar zz/alt-fn-map (make-sparse-keymap) "f1-f12 => M-f1/f12")
(define-key help-map [f12] zz/alt-fn-map)

;;f1-f5 => ctrl-x f1/f12
(defvar zz/ctrl-x-fn-map (make-sparse-keymap) "f1-f5 => ctrl-x f1/f12")
(define-key help-map (kbd "<f5>") zz/ctrl-x-fn-map)

;;f1-f5 => ctrl-c f1/f12
(defvar zz/ctrl-c-fn-map (make-sparse-keymap) "f1-f5 => ctrl-c f1/f12")
(define-key help-map (kbd "<f6>") zz/ctrl-c-fn-map)

;;group define fn key
(defun define-fn-key (fn-name &optional fn s-fn c-fn m-fn ctrl-x-fn ctrl-c-fn)
  (when fn        (define-key zz/fn-map        fn-name fn))
  (when s-fn      (define-key zz/shift-fn-map  fn-name s-fn))
  (when c-fn      (define-key zz/ctrl-fn-map   fn-name c-fn))
  (when m-fn      (define-key zz/alt-fn-map    fn-name m-fn))
  (when ctrl-x-fn (define-key zz/ctrl-x-fn-map fn-name ctrl-x-fn))
  (when ctrl-c-fn (define-key zz/ctrl-c-fn-map fn-name ctrl-c-fn)))

;;define fn key setting
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
    'kill-this-buffer                ;;[f4]
    'undo-kill-buffer                ;;[S-f4]
    'highlight-symbol-next           ;;[C-f4]
    'highlight-symbol-prev           ;;[M-f4]
    'recentf-open-files              ;;C-x f4
    'recentf-open-files-compl        ;;C-c f4
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
    (if-ms-windows
     'multi-shell-new
     'get-term)                      ;;[f6]
    (if-ms-windows
     'multi-shell-current-directory
     'multi-term-dedicated-toggle)   ;;[S-f6]
    (if-ms-windows
     'multi-shell-next
     'multi-term-next)               ;;[C-f6]
    (if-ms-windows
     'multi-shell-prev
     'multi-term-prev)               ;;[M-f6]    
    (if-ms-windows
     'switch-to-shell
     'switch-to-term)                ;;C-x f6
    (unless-ms-windows
     'switch-term-and-text)          ;;C-c f6
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
    (lambda ()
      (interactive)
      (start-shell "*shell*"))       ;;[f9]
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
    'my-unicad-switch                ;;C-x f12
    'my-os-file-switch               ;;C-x f12
    )

;;apply fn-key setting
(apply-keys-to-map
 global-map
 (list
  ;;[f1] for help-map
  [S-f1]            (lookup-key zz/shift-fn-map  "1")
  [C-f1]            (lookup-key zz/ctrl-fn-map   "1")
  [M-f1]            (lookup-key zz/alt-fn-map    "1")
  (kbd "C-x <f1>")  (lookup-key zz/ctrl-x-fn-map "1")
  (kbd "C-c <f1>")  (lookup-key zz/ctrl-c-fn-map "1")
                   
  [f2]              (lookup-key zz/fn-map        "2")
  [S-f2]            (lookup-key zz/shift-fn-map  "2")
  [C-f2]            (lookup-key zz/ctrl-fn-map   "2")
  [M-f2]            (lookup-key zz/alt-fn-map    "2")
  (kbd "C-x <f2>")  (lookup-key zz/ctrl-x-fn-map "2")
  (kbd "C-c <f2>")  (lookup-key zz/ctrl-c-fn-map "2")
                   
  [f3]              (lookup-key zz/fn-map        "3")
  [S-f3]            (lookup-key zz/shift-fn-map  "3")
  [C-f3]            (lookup-key zz/ctrl-fn-map   "3")
  [M-f3]            (lookup-key zz/alt-fn-map    "3")
  (kbd "C-x <f3>")  (lookup-key zz/ctrl-x-fn-map "3")
  (kbd "C-c <f3>")  (lookup-key zz/ctrl-c-fn-map "3")

  ;;[f4] for f4-map
  [S-f4]            (lookup-key zz/shift-fn-map  "4")
  [C-f4]            (lookup-key zz/ctrl-fn-map   "4")
  [M-f4]            (lookup-key zz/alt-fn-map    "4")
  (kbd "C-x <f4>")  (lookup-key zz/ctrl-x-fn-map "4")
  (kbd "C-c <f4>")  (lookup-key zz/ctrl-c-fn-map "4")
                   
  [f5]              (lookup-key zz/fn-map        "5")
  [S-f5]            (lookup-key zz/shift-fn-map  "5")
  [C-f5]            (lookup-key zz/ctrl-fn-map   "5")
  [M-f5]            (lookup-key zz/alt-fn-map    "5")
  (kbd "C-x <f5>")  (lookup-key zz/ctrl-x-fn-map "5")
  (kbd "C-c <f5>")  (lookup-key zz/ctrl-c-fn-map "5")
                   
  [f6]              (lookup-key zz/fn-map        "6")
  [S-f6]            (lookup-key zz/shift-fn-map  "6")
  [C-f6]            (lookup-key zz/ctrl-fn-map   "6")
  [M-f6]            (lookup-key zz/alt-fn-map    "6")
  (kbd "C-x <f6>")  (lookup-key zz/ctrl-x-fn-map "6")
  (kbd "C-c <f6>")  (lookup-key zz/ctrl-c-fn-map "6")
                   
  [f7]              (lookup-key zz/fn-map        "7")
  [S-f7]            (lookup-key zz/shift-fn-map  "7")
  [C-f7]            (lookup-key zz/ctrl-fn-map   "7")
  [M-f7]            (lookup-key zz/alt-fn-map    "7")
  (kbd "C-x <f7>")  (lookup-key zz/ctrl-x-fn-map "7")
  (kbd "C-c <f7>")  (lookup-key zz/ctrl-c-fn-map "7")
                   
  [f8]              (lookup-key zz/fn-map        "8")
  [S-f8]            (lookup-key zz/shift-fn-map  "8")
  [C-f8]            (lookup-key zz/ctrl-fn-map   "8")
  [M-f8]            (lookup-key zz/alt-fn-map    "8")
  (kbd "C-x <f8>")  (lookup-key zz/ctrl-x-fn-map "8")
  (kbd "C-c <f8>")  (lookup-key zz/ctrl-c-fn-map "8")
                   
  [f9]              (lookup-key zz/fn-map        "9")
  [S-f9]            (lookup-key zz/shift-fn-map  "9")
  [C-f9]            (lookup-key zz/ctrl-fn-map   "9")
  [M-f9]            (lookup-key zz/alt-fn-map    "9")
  (kbd "C-x <f9>")  (lookup-key zz/ctrl-x-fn-map "9")
  (kbd "C-c <f9>")  (lookup-key zz/ctrl-c-fn-map "9")

  [f10]             (lookup-key zz/fn-map        "0")
  [S-f10]           (lookup-key zz/shift-fn-map  "0")
  [C-f10]           (lookup-key zz/ctrl-fn-map   "0")
  [M-f10]           (lookup-key zz/alt-fn-map    "0")
  (kbd "C-x <f10>") (lookup-key zz/ctrl-x-fn-map "0")
  (kbd "C-c <f10>") (lookup-key zz/ctrl-c-fn-map "0")

  [f11]             (lookup-key zz/fn-map        "-")
  [S-f11]           (lookup-key zz/shift-fn-map  "-")
  [C-f11]           (lookup-key zz/ctrl-fn-map   "-")
  [M-f11]           (lookup-key zz/alt-fn-map    "-")
  (kbd "C-x <f11>") (lookup-key zz/ctrl-x-fn-map "-")
  (kbd "C-c <f11>") (lookup-key zz/ctrl-c-fn-map "-")

  [f12]             (lookup-key zz/fn-map        "=")
  [S-f12]           (lookup-key zz/shift-fn-map  "=")
  [C-f12]           (lookup-key zz/ctrl-fn-map   "=")
  [M-f12]           (lookup-key zz/alt-fn-map    "=")
  (kbd "C-x <f12>") (lookup-key zz/ctrl-x-fn-map "=")
  (kbd "C-c <f12>") (lookup-key zz/ctrl-c-fn-map "=")
  ))


(provide 'fn-setting)

;;; fn-setting.el ends here
