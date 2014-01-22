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
	  (lambda ()
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
          (lambda ()
            (if (saved-session)
                (if (y-or-n-p "Restore desktop? ")
                    (session-restore)))))

;;save desktop when exit
;(add-hook 'kill-emacs-hook (lambda () (session-save)))

;;Filecode Autoprocess
;;distct with mpg123
(require 'unicad)

;;; ASCII table
(autoload 'ascii-table "ascii-table" "ASCII TABLE" t)

;;display of line numbers with M-x linum-mode.
(require 'nlinum)

;;redo+ fixed for emacs > 24.3
(require 'redo+)
(global-set-key [M-S-backspace] 'redo)

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

;(defun kill-buffer-when-exit ()
;  "Close assotiated buffer when a process exited"
;  (let ((current-process (ignore-errors (get-buffer-process (current-buffer)))))
;    (when current-process
;      (set-process-sentinel current-process
;                            (lambda (watch-process change-state)
;                               (when (string-match "//(finished//|exited//)" change-state)
;                                (kill-buffer (process-buffer watch-process))))))))

;(add-hook 'gdb-mode-hook 'kill-buffer-when-exit)
;(add-hook 'shell-mode-hook 'kill-buffer-when-exit)

;;tramp setting
(require 'tramp)      
(setq tramp-default-method (if-ms-windows "scpx" "sshx"))
(setq tramp-unified-filenames t)
(setq tramp-auto-save-directory "~/var/tramp")

;;ange-ftp
(setq ange-ftp-generate-anonymous-password "zjd-405@163.com")
(setq ange-ftp-default-user t)

;;https://github.com/nonsequitur/smex/
(require 'smex)  
(smex-initialize)

;;mulit-occur
;;isearch press M-o will list match in current buffer
;;M-O(not zero) will list match in all buffers
;(require 'color-moccur)
;(require 'moccur-edit)

;;bookmark setting
;(require 'bm)
;(setq bm-marker 'bm-marker-left)
;(setq bm-highlight-style 'bm-highlight-only-fringe)

;;show *bm-bookmarks* buffer
;(defun bm-menu-show ()
;  (interactive)
;  (bm-show-all)
;  (delete-other-windows))

;;bookmark in file setting
(require 'breadcrumb)

;;uing iswitchb-mode
(require 'iswitchb)
(iswitchb-mode t)
(setq iswitchb-buffer-ignore
      '("^ "
        "^\*Buffer"
        "^\*Completions\*"
        "^\*Ido Completions\*"
        "^\*Quail Completions\*"
        "^TAGS"
        ))

;;Using the arrow keys to select a buffer
(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
	      (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	    '(("<right>" . iswitchb-next-match)
	      ("<left>"  . iswitchb-prev-match)
	      ("<up>"    . ignore)
	      ("<down>"  . ignore)
          )))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;update when kill buffer in iswitchb
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

;;on duplicate filenames, show path names, not foo.x<2>, foo.x<3>, etc.
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t)

;;ibuffer setting
(setq ibuffer-saved-filter-groups
      (quote (("Default"
               ("Dired" (mode . dired-mode))
               ("Emacs" (or
                          (name . "^\\*.*\\*$")
                          (name . "^ ")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "Default")))

;;switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent buffer name"
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;alias operate setting
(fset 'rm 'delete-file)
(fset 'mv 'rename-file)
(fset 'cp 'copy-file)
(fset 'mkdir 'make-directory)
(fset 'rmdir 'delete-directory)

;;disabled commands
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

;;register-list
(require 'register-list)

;;nxml-mode is a more powerful xml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;;vim script and config file mode
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))


(provide 'other-setting)

;;; other-setting.el ends here
