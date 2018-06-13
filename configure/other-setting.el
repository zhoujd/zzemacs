;;;; other-setting.el --- other setting file
;;;

(zz-load-path "site-lisp")
(zz-load-path "elisp")

;;helm - anything
(zz-load-path "site-lisp/helm")
(require 'helm-files)
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

;;redo+
(require 'redo+)
(global-set-key [M-S-backspace] 'redo)
(global-set-key (kbd "C-?") 'redo)

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
;;http://www.emacswiki.org/emacs/TrampMode
;;http://lifegoo.pluskid.org/wiki/EmacsTRAMP.html
;;http://stackoverflow.com/questions/1134149/emacs-remote-shell
(when (require 'tramp nil 'noerror)            ;;run if tramp exists && not loaded yet.
  (setq tramp-shell-prompt-pattern             ;;to work with zsh prompt
        "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
  (setq tramp-default-method (if-ms-windows "plink" "sshx")) ;;faster, as is "scp"
  (setq tramp-debug-buffer t)
  (setq tramp-verbose 10))

;;ange-ftp
(setq ange-ftp-generate-anonymous-password "zachary.zhou@hotmail.com")
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
(require 'iswitchb-filter)
(iswitchb-mode t)

(setq iswitchb-buffer-ignore
      '("^ "
        "^\*Buffer"
        "^\*Completions\*"
        "^\*Ido Completions\*"
        "^\*Quail Completions\*"
        "^TAGS"
        ))

;;using the arrow keys to select a buffer
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . isb-filter-prev)
          ("<down>"  . isb-filter-next)
          ("\C-o"    . isb-show-emacs)
          ("\C-p"    . isb-show-dired)
          ("\C-v"    . isb-show-common)
          ("\C-u"    . isb-rescan)
          )))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;update when kill buffer in iswitchb
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (isb-rescan))

;;on duplicate filenames, show path names, not foo.x<2>, foo.x<3>, etc.
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t)

;;ibuffer setting
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("Emacs" (or
                          (name . "^\\*.*\\*$")
                          (name . "^\\*.*\\*<[0-9]+>$")
                          (name . "^ ")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

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
(zz-load-path "site-lisp/smartparens")
(require 'smartparens-config)
;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;;register-list
(require 'register-list)

;;nxml-mode is a more powerful xml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;;vim script and config file mode
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;;minimap show left
;(require 'minimap)

;;igrep
(require 'igrep)

;;escreen
(require 'escreen)
(escreen-install)

;;winring
(require 'winring)
(winring-initialize)

;;grep+
(require 'grep+)

;;powerline
;(require 'powerline)
;(set-face-attribute 'mode-line nil :background "#bdbdbd" :box nil)

;;iedit
(zz-load-path "site-lisp/iedit")
(require 'iedit)


(provide 'other-setting)

;;; other-setting.el ends here
