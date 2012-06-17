;;;; other-setting.el --- other setting file
;;;

(zz-load-path "site-lisp")

;;session + desktop
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file-coding-system 'utf-8)

(load "desktop")
(desktop-load-default)
(desktop-read)

;;Filecode Autoprocess
;;distct with mpg123
(require 'unicad)

;;; ASCII table
(autoload 'ascii-table "ascii-table" "ASCII TABLE" t)

;;display of line numbers with M-x linum-mode.
(require 'linum)
;(global-linum-mode t)

(require 'redo+)

;;tabbar mode
(if window-system
    (require 'tabbar-ruler)
    (require 'tabbar))

;;Excluded buffers in tabbar
;(setq EmacsPortable-excluded-buffers '("*Messages*" "*Completions*" "*ESS*"))

; turn on the tabbar
(tabbar-mode t)
; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”,
;“User Buffer”.
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    (t
     "User Buffer"
     )
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(defvar my-font-en-name "DejaVu Sans Mono")
(if window-system
    (set-face-attribute 'tabbar-default nil
                        :family     my-font-en-name
                        :background "gray80"
                        :foreground "gray30"
                        :height 0.8
                        :box nil
                        ))
(set-face-attribute 'tabbar-button nil
                    :inherit 'tabbar-default
                    :box '(:line-width 1 :color "gray65")
                    )
(set-face-attribute 'tabbar-selected nil
                    :inherit 'tabbar-default
                    :foreground "DarkGreen"
                    :background "LightGoldenrod"
                    :box '(:line-width 1 :color "DarkGoldenrod")
                    :weight 'bold
                    )
(set-face-attribute 'tabbar-unselected nil
                    :inherit 'tabbar-default
                    :box '(:line-width 1 :color "gray70")
                    )
(set-face-attribute 'tabbar-button nil
                    :inherit 'tabbar-default
                    :box nil)
(set-face-attribute 'tabbar-separator nil
                    :background "grey50"
                    :foreground "grey50"
                    :height 0.8)

;; switch buffer exclude temp buffer
;; swbuff Ctrl+(left/right)
(require 'swbuff)
(setq swbuff-exclude-buffer-regexps  '("^ " "\\*.*\\*"))
(setq swbuff-status-window-layout 'scroll)
(setq swbuff-clear-delay 2)
(setq swbuff-separator "|")
(setq swbuff-window-min-text-height 1)

;; mswbuff  Ctrl+(,/.)
(require 'mswbuff)
(setq mswbuff-exclude-buffer-regexps  '("^[^\*]"))
(setq mswbuff-status-window-layout 'scroll)
(setq mswbuff-clear-delay 2)
(setq mswbuff-separator "|")
(setq mswbuff-window-min-text-height 1)

;; space tab show
;(require 'jaspace)
;(setq jaspace-alternate-eol-string "\xab\n")
;(setq jaspace-highlight-tabs t) ; highlight tabs ; ...

(require 'blank-mode)
;(defvar blank-bg-color "#0C1021")
;(defvar blank-fg-color "gray30")

(defvar blank-bg-color (background-color-at-point))
(defvar blank-fg-color "gray20")

(set-face-attribute 'blank-space nil
                    :background blank-bg-color
                    :foreground blank-fg-color
                    )

(set-face-attribute 'blank-tab nil
                    :background blank-bg-color
                    :foreground blank-fg-color
                    )
(set-face-attribute 'blank-newline nil
                    :background blank-bg-color
                    :foreground blank-fg-color
                    )

(require 'what-char)
(require 'xray)

;; Support for marking a rectangle of text with highlighting.
(require 'rect-mark)
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)

(require 'fill-column-indicator)
(setq fci-rule-color "gray30")

;;paren switch
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


; on duplicate filenames, show path names, not foo.x<2>, foo.x<3>, etc.
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style 'reverse)
 '(uniquify-after-kill-buffer-p t))

(defun kill-buffer-when-exit ()
  "Close assotiated buffer when a process exited"
  (let ((current-process (ignore-errors (get-buffer-process
                                         (current-buffer)))))
    (when current-process
      (set-process-sentinel current-process
                            (lambda (watch-process change-state)
                              (when (string-match "//(finished//|exited//)" change-state)
                                (kill-buffer (process-buffer watch-process))))))))

(add-hook 'gdb-mode-hook 'kill-buffer-when-exit)
(add-hook 'shell-mode-hook 'kill-buffer-when-exit)

;;tramp setting
(require 'tramp)
(cond ((or (eq window-system 'w32)
           (eq window-system 'win32))
       (setq tramp-default-method "scpx"))
      (t
       (setq tramp-default-method "sshx")))

;;https://github.com/nonsequitur/smex/
;(require 'smex)  
;(smex-initialize)
;(global-set-key (kbd "M-x") 'smex)  
;(global-set-key (kbd "M-X") 'smex-major-mode-commands)  
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;mulit-occur
;; isearch 时按 M-o，列出当前 buffer 的匹配结果;  
;; M-O(大写o) 列出所有 buffer 里的匹配结果 
;(require 'color-moccur)
;(require 'moccur-edit)

;;http://emacswiki.org/emacs/Evil
;(zz-load-path "site-lisp/evil")
;(require 'evil)

;;bookmark
(require 'breadcrumb)

;;http://www.emacswiki.org/emacs/w32-browser.el
(when (or (eq window-system 'w32)
          (eq window-system 'win32))
  (require 'w32-browser)
  (eval-after-load "dired"
    '(define-key dired-mode-map [C-f4] (lambda ()
                                         (interactive)
                                         (w32-browser
                                          (dired-replace-in-string
                                           "/" "\\"
                                           (dired-get-filename))))))
  )

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent buffer name"
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;win32 find grep set
(if (or (eq window-system 'w32)
        (eq window-system 'win32))
    (progn
      (setq find-program "\"find2.exe\"")
      (setq grep-program "\"grep.exe\"")
      ))

(fset 'rm 'delete-file)
(fset 'mv 'rename-file)
(fset 'cp 'copy-file)
(fset 'mkdir 'make-directory)
(fset 'rmdir 'delete-directory)

;;Disabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(provide 'other-setting)

;;; other-setting.el ends here
