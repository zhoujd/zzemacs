;;;; other-setting.el --- other setting file
;;;

(zz-load-path "site-lisp")

;;Filecode Autoprocess
;;distct with mpg123
(require 'unicad)
;(unicad-disable)

;;my unicad enable/disable switch
(defun my-unicad-switch ()
  "unicad enable/disable switch"
  (interactive)
  (if (eq t unicad-global-enable)
    (progn (setq unicad-global-enable nil)
           (message "unicad is disabled"))
    (progn (setq unicad-global-enable t)
           (message "unicad is enabled"))))

;;using spaces repalce tabs
(defun untabify-buffer ()
  "untabify whole buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (save-buffer))

;;using tabs repalce spaces
(defun tabify-buffer ()
  "tabify whole buffer"
  (interactive)
  (tabify (point-min) (point-max))
  (save-buffer))

;;File coding for Unix Dos Mac switcher
(defvar my-os-flag 0)
(defun my-os-file-switch ()
    "for unix dos max"
    (interactive)
    (if (> my-os-flag 2)
      (setq my-os-flag 0))
    (cond ((eq my-os-flag 0)
           (set-buffer-file-coding-system 'unix 't)
           (message "end line with LF"))
          ((eq my-os-flag 1)
           (set-buffer-file-coding-system 'dos 't)
           (message "end line with CRLF"))
          ((eq my-os-flag 2)
           (set-buffer-file-coding-system 'mac 't)
           (message "end line with CR")))
    (setq my-os-flag (+ my-os-flag 1)))


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

;;; Maximum Windows Frame
(defvar my-fullscreen-p t "Check if fullscreen is on or off")
(defun my-sub-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                           'fullboth)))

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
      (run-with-idle-timer 0.1 nil 'my-sub-fullscreen)))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
      (run-with-idle-timer 0.1 nil 'my-sub-fullscreen)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (my-non-fullscreen)
	(my-fullscreen)))


;;display current buffer name
(defun display-buffer-name ()
  (interactive)
  (message (buffer-file-name (current-buffer))))


;;paren switch
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;set file to utf-8
(defun my-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer)
  (message "this file to utf-8 ok!"))

(defun to-scratch ()
  "switch to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  (message "switch to *scratch*"))

(defun to-compilation ()
  "switch to *compilation* buffer"
  (interactive)
  (switch-to-buffer "*compilation*")
  (message "switch to *compilation*"))

(defun edit-with-gvim()
  "Edit current buffer file with gvim"
  (interactive)
  (start-process  "gvim"
                  nil
                  "gvim"
                  (display-buffer-name)))

;;popup term
(if (or (eq window-system 'w32)
        (eq window-system 'win32))
    (setq popup-terminal-command '("cmd" "/c" "start"))
    (setq popup-terminal-command '("gnome-terminal")))
(defun popup-term ()
  (interactive)
  (apply 'start-process "terminal" nil popup-terminal-command)
  )

;;go to last buffer
(defun my-last-buffer-go ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun line-to-top-of-window ()
    (interactive)
    (recenter 0))

;; used in my ion C-F2 key binding. run before shutting down X!
(defun delete-all-x-frames ()
  (mapcar (lambda (frame) (if (eq 'x (frame-live-p frame))
                              (delete-frame frame)))
          (frame-list)))

(defun make-my-frames ()
  (interactive)
  (make-frame '((name . "emacs main")))
  (make-frame '((name . "emacs secondary")))
  )

; on duplicate filenames, show path names, not foo.x<2>, foo.x<3>, etc.
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style 'reverse)
 '(uniquify-after-kill-buffer-p t))


;; create shell 
(defun shell-create-by-name (shell-name)
 (interactive)
 (shell shell-name)
 (delete-other-windows))

;;http://www.emacswiki.org/emacs/multi-shell.el
;(require 'multi-shell)
;;http://www.emacswiki.org/emacs/MultiTerm
;;http://code.google.com/p/dea/source/browse/trunk/my-lisps/multi-term-settings.el
(require 'multi-term)
(setq multi-term-switch-after-close nil)
(setq multi-term-dedicated-select-after-open-p t)
(setq multi-term-program "/bin/bash")
(add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-escape))

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
	(if (eq 'term-mode (with-current-buffer (car l) major-mode))
	    (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
	(if (or (not b) (eq 'term-mode major-mode))
	    (multi-term)
        (switch-to-buffer b))))

(defun it-multi-term-dedicated-toggle ()
  "jump back to previous location after toggling ded term off"
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (progn
        (multi-term-dedicated-toggle)
        (switch-to-buffer-other-window old-buf))
      (progn
        (setq old-buf (current-buffer))
        (multi-term-dedicated-toggle))
      )
  )

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
(zz-load-path "site-lisp/evil")
(require 'evil)

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

;;file transform
(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET" 
  (interactive "*b") 
  (save-excursion 
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

;;occur setting
(defun my-occur (&optional arg)
  "Switch to *Occur* buffer, or run `occur'.
   Without a prefix argument, switch to the buffer.
   With a universal prefix argument, run occur again.
   With a numeric prefix argument, run occur with NLINES
   set to that number."
  (interactive "P")
  (if (and (not arg) (get-buffer "*Occur*"))
      (switch-to-buffer "*Occur*")
      (occur (read-from-minibuffer "Regexp: ")
             (if (listp arg) 0 arg))))

(defun occur-at-point()
  "point at word"
  (interactive)
  (if (thing-at-point 'word)
      (occur (thing-at-point 'word))
      (call-interactively 'occur)))

(provide 'other-setting)

;;; other-setting.el ends here
