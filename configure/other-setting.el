;;;; other-setting.el --- other setting file
;;;

(zz-load-path "site-lisp")

;;helm - anything
(zz-load-path "site-lisp/helm")
(require 'helm-config)

;;session + desktop
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file-coding-system 'utf-8)
(desktop-load-default)

;; use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(setq desktop-load-locked-desktop t)

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
	  '(lambda ()
	     ;; desktop-remove clears desktop-dirname
	     (setq desktop-dirname-tmp desktop-dirname)
	     (desktop-remove)
	     (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
             (if (y-or-n-p "Restore desktop? ")
                 (session-restore)))))

;;save desktop when exit
;(add-hook 'kill-emacs-hook '(lambda () (session-save)))

;;Filecode Autoprocess
;;distct with mpg123
(require 'unicad)

;;; ASCII table
(autoload 'ascii-table "ascii-table" "ASCII TABLE" t)

;;display of line numbers with M-x linum-mode.
(require 'linum)

;;redo+ failed at emacs 24.3
(if (and (=  emacs-major-version 24)
         (>= emacs-minor-version 3))
    (require 'redo)
    (require 'redo+))

;;tabbar mode
;(if window-system (require 'tabbar-ruler) (require 'tabbar))
;(require 'tabbar-rose)
(require 'tabbar)

;;turn on the tabbar
(tabbar-mode t)
(setq tabbar-mwheel-mode nil)

;;Excluded buffers in tabbar
(setq tabbar-excluded-buffers '("*Messages*" "*Completions*" "*ESS*" "*Pymacs*" "*WoMan-Log*"))

(defun tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or *, when they are not
visiting a file.  The current buffer is always included."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                      ;; Always include the current buffer.
                      ((eq (current-buffer) b) b)
                      ((buffer-file-name b) b)
                      ((member (buffer-name b) tabbar-excluded-buffers) nil)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      ((buffer-live-p b) b)))
                (buffer-list))))

(setq tabbar-buffer-list-function 'tabbar-buffer-list)

;;define all tabs to be one of 2 possible groups: “Emacs Buffer”, “User Buffer”.
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((or (string-match "\\*.*\\*"  (buffer-name))
         (string-match "^ "  (buffer-name)))
     "Emacs Buffer"
     )
    (t
     "User Buffer"
     )
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;;tabbar font name setting
(if (boundp 'my-font-en-name)
    (setq tabbar-font-name (car (split-string my-font-en-name)))
    (setq tabbar-font-name "Consolas"))

(if window-system
    (setq tabbar-bgcolor "#AAAAAA")
    (setq tabbar-bgcolor "#75507B"))

(set-face-attribute 'tabbar-default nil
                    :inherit    nil
                    :weight    'normal
                    :width     'normal
                    :slant     'normal
                    :underline  nil
                    :strike-through nil
                    :stipple    nil
                    :background tabbar-bgcolor
                    :foreground "black"
                    :box    nil
                    :family tabbar-font-name)
(set-face-attribute 'tabbar-selected nil
                    :background "LightGoldenrod"
                    :foreground "DarkGreen"
                    :inherit    'tabbar-default 
                    :box (list :line-width 2 :color "LightGoldenrod" :style nil)
                    :weight 'bold)
(set-face-attribute 'tabbar-unselected nil
                    :inherit    'tabbar-default
                    :background tabbar-bgcolor
                    :box (list :line-width 2 :color tabbar-bgcolor :style nil))
(set-face-attribute 'tabbar-button nil
                    :inherit 'tabbar-default
                    :box nil)
(set-face-attribute 'tabbar-separator nil
                    :background "grey50"
                    :foreground "grey50"
                    :height 1)

;; space tab show
;(require 'jaspace)
;(setq jaspace-alternate-eol-string "\xab\n")
;(setq jaspace-highlight-tabs t) ; highlight tabs ; ...

(require 'blank-mode)
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
(define-key ctl-x-map "r\C-y" 'yank-rectangle)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

(require 'fill-column-indicator)
(setq fci-rule-color "gray30")

;;paren switch
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;on duplicate filenames, show path names, not foo.x<2>, foo.x<3>, etc.
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style 'reverse)
 '(uniquify-after-kill-buffer-p t))

;(defun kill-buffer-when-exit ()
;  "Close assotiated buffer when a process exited"
;  (let ((current-process (ignore-errors (get-buffer-process (current-buffer)))))
;    (when current-process
;      (set-process-sentinel current-process
;                            (lambda (watch-process change-state)
;                              (when (string-match "//(finished//|exited//)" change-state)
;                                (kill-buffer (process-buffer watch-process))))))))

;(add-hook 'gdb-mode-hook 'kill-buffer-when-exit)
;(add-hook 'shell-mode-hook 'kill-buffer-when-exit)

;;tramp setting
(require 'tramp)
(if-ms-windows       
 (setq tramp-default-method "scpx")
 (setq tramp-default-method "sshx"))

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

;;bookmark
;(require 'bm)
;(setq bm-marker 'bm-marker-left)
;(setq bm-highlight-style 'bm-highlight-only-fringe)

;;show *bm-bookmarks* buffer
;(defun bm-menu-show ()
;  (interactive)
;  (bm-show-all)
;  (delete-other-windows))

(require 'breadcrumb)

;;http://www.emacswiki.org/emacs/w32-browser.el
(when-ms-windows  
  (require 'w32-browser)
  (eval-after-load "dired"
    '(define-key dired-mode-map [C-f4] (lambda ()
                                         (interactive)
                                         (w32-browser
                                          (dired-replace-in-string
                                           "/" "\\"
                                           (dired-get-filename)))))))

;;Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent buffer name"
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

(fset 'rm 'delete-file)
(fset 'mv 'rename-file)
(fset 'cp 'copy-file)
(fset 'mkdir 'make-directory)
(fset 'rmdir 'delete-directory)

;;Disabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;;eproject manage
;(zz-load-path "site-lisp/eproject")
;(require 'eproject)

;;http://nschum.de/src/emacs/highlight-symbol/
(require 'highlight-symbol)

;;maxframe
(require 'maxframe)

;;bookmark+
(zz-load-path "site-lisp/bookmarkplus")
(require 'bookmark+)
(custom-set-faces
 '(bmkp-heading ((t (:foreground "White"))))
 '(bmkp-local-file-without-region ((t nil))))

;;expand-region
(zz-load-path "site-lisp/expand-region")
(require 'expand-region)

;;a modern list operater
(zz-load-path "site-lisp/dash")
(require 'dash)

;;smartparens
(when-emacs24-3
 (zz-load-path "site-lisp/smartparens")
 (require 'smartparens-config))

(provide 'other-setting)

;;; other-setting.el ends here
