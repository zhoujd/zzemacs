;;;; other-setting.el --- other setting file
;;;

(zz:load-path "site-lisp")
(zz:load-path "elisp")

;; session + desktop
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file-coding-system 'utf-8)
(when (< emacs-major-version 27)
  (desktop-load-default))

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

(defun zz:saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; restore the desktop manually
(defun zz:session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (zz:saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun zz:session-save ()
  "Save an emacs session."
  (interactive)
  (if (zz:saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
      (desktop-save-in-desktop-dir)
    (message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
          (lambda ()
            (if (zz:saved-session)
                (if (y-or-n-p "Restore desktop? ")
                    (zz:session-restore)))))

;;save desktop when exit
;(add-hook 'kill-emacs-hook (lambda () (zz:session-save)))

;;Filecode Autoprocess
;;distct with mpg123
(require 'unicad)

;;; ASCII table
(autoload 'ascii-table "ascii-table" "ASCII TABLE" t)

;;display of line numbers with M-x linum-mode.
(require 'nlinum)

;;redo+
(require 'redo+)
(defkeys-map global-map
  ([M-S-backspace] 'redo)
  ((kbd "C-?")     'redo))

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
(defkeys-map ctl-x-map
  ("r\C-@"     'rm-set-mark)
  ([?r ?\C-\ ] 'rm-set-mark)
  ("r\C-x"     'rm-exchange-point-and-mark)
  ("r\C-w"     'rm-kill-region)
  ("r\M-w"     'rm-kill-ring-save)
  ("r\C-y"     'yank-rectangle))
(defkeys-map global-map
  ([S-down-mouse-1] 'rm-mouse-drag-region))

(require 'fill-column-indicator)
(setq fci-rule-color "gray30")

;;paren switch
(defun zz:match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;;tramp setting
;;Press C-l ido-reread-directory while in the process of finding a file.
;;C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file RET
;;C-x C-f /multi:ssh:foo@remote:ssh:bar@secret:~/.emacs
(require 'tramp)
(add-to-list 'tramp-methods
             '("sshz"
               (tramp-login-program        "ssh")
               (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
                                            ("-e" "none") ("-X") ("%h")))
               (tramp-async-args           (("-q")))
               (tramp-remote-shell         "/bin/sh")
               (tramp-remote-shell-login   ("-l"))
               (tramp-remote-shell-args    ("-c"))
               (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                            ("-o" "UserKnownHostsFile=/dev/null")
                                            ("-o" "StrictHostKeyChecking=no")
                                            ("-o" "ForwardX11=yes")))
               (tramp-default-port         22)))
(tramp-set-completion-function "sshz" tramp-completion-function-alist-ssh)
(setq tramp-default-method (if-ms-windows "plink" "sshz"))
;;speed up tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)

;;ange-ftp
(setq ange-ftp-generate-anonymous-password "zchrzhou@gmail.com")
(setq ange-ftp-default-user t)

;;https://github.com/nonsequitur/smex/
(require 'smex)
(smex-initialize)

;;bookmark setting
(require 'bm)
(setq bm-marker 'bm-marker-left)
(setq bm-highlight-style 'bm-highlight-only-fringe)

;show *bm-bookmarks* buffer
(defun zz:bm-menu-show ()
  (interactive)
  (bm-show-all)
  (delete-other-windows))

;;on duplicate filenames, show path names, not foo.x<2>, foo.x<3>, etc.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t)

;;ibuffer setting
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Helm" (predicate string-match "Hmm" mode-name))
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

;;ibuffer ido
(defun ibuffer-ido-find-file (file &optional wildcards)
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory
           (let ((buf (ibuffer-current-buffer)))
             (if (buffer-live-p buf)
                 (with-current-buffer buf
                   default-directory)
                 default-directory))))
     (list (ido-read-file-name "Find file: " default-directory) t)))
  (find-file file wildcards))

;;ibuffer keymap
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map (kbd "C-x C-f") 'ibuffer-ido-find-file)
            (define-key ibuffer-mode-map (kbd "C-x f") 'ibuffer-find-file)
            ))

;;ido-find-file
(defun zz:find-file ()
  (interactive)
  (ido-mode t)
  (ido-find-file))

(defun zz:dired ()
  (interactive)
  (ido-mode t)
  (ido-dired))

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

;;http://nschum.de/src/emacs/highlight-symbol/
(require 'highlight-symbol)

;;bookmark plus
;;https://www.emacswiki.org/emacs/BookmarkPlus
(zz:load-path "site-lisp/bookmarkplus")
(require 'bookmark+)
;;auto-save bookmarks flag, toggle this option using 'M-~’
(setq bookmark-save-flag t)
(custom-set-faces
 '(bmkp-heading ((t (:foreground "White"))))
 '(bmkp-local-file-without-region ((t nil))))
(setq bmkp-bmenu-state-file "~/.emacs.d/.emacs-bmk-bmenu-state.el"
      bmkp-bmenu-commands-file "~/.emacs.d/.emacs-bmk-bmenu-commands.el")

;;expand-region
(zz:load-path "site-lisp/expand-region")
(require 'expand-region)

;;smartparens
(zz:load-path "site-lisp/smartparens")
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

;;igrep
(require 'igrep)

;;escreen
(require 'escreen)
(escreen-install)

;;grep+
(require 'grep+)

;;iedit
(zz:load-path "site-lisp/iedit")
(require 'iedit)
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
              ;; `current-word' can of course be replaced by other
              ;; functions.
              (narrow-to-defun)
              (iedit-start (current-word) (point-min) (point-max)))))))

;;emacs-calfw
;;https://en.wikipedia.org/wiki/Week#Week_numbering
(zz:load-path "site-lisp/emacs-calfw")
(require 'calfw)

;;ssh-tunnels
(require 'ssh-tunnels)

;;backlight
(require 'backlight)

;;whitespace cleanup mode
(require 'whitespace-cleanup-mode)
(dolist (hook
         (list
          'java-mode-hook
          'c++-mode-hook
          'python-mode-hook
          'c-mode-hook
          'perl-mode-hook
          'php-mode-hook
          'emacs-lisp-mode-hook
          'markdown-mode-hook
          'yaml-mode-hook
          'lisp-mode-hook
          'shell-script-mode-hook
          'org-mode-hook
          ))
  (add-hook hook 'whitespace-cleanup-mode))

;;https://github.com/emacsorphanage/anzu
(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => "))

(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;;https://framagit.org/steckerhalter/discover-my-major
(require 'discover-my-major)

;;https://github.com/mickeynp/discover.el
(require 'discover)

;;https://github.com/xuchunyang/translate-shell.el
(require 'translate-shell)

;;https://github.com/emacs-straight/disk-usage
(require 'disk-usage)

;;Ubuntu: apt install ripgrep
(require 'ripgrep)
(require 'projectile-ripgrep)

;;logview
(zz:load-path "site-lisp/datetime")
(require 'logview)

;;google this
(require 'google-this)
(google-this-mode t)

;;neotree
(zz:load-path "site-lisp/neotree")
(require 'neotree)

;;trash
(require 'trashed)
(setq trashed-buffer-name "*Trash Can*")

;;vkill
(require 'vkill)

;;git-annex
(require 'git-annex)

;;mood-line
(require 'mood-line)
(mood-line-mode t)

;;ssh-config-mode
(require 'ssh-config-mode)
(add-to-list 'auto-mode-alist '("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode))

;;M-x world-time-list
(require 'world-time-mode)

;;flx ido
(require 'flx-ido)
(flx-ido-mode t)

;;ido-better-flex
;(require 'ido-better-flex)
;(ido-better-flex/enable)

;;buffer-flip
(require 'buffer-flip)
;;transient keymap used once cycling starts
(setq buffer-flip-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-<tab>")   'buffer-flip-forward)
        (define-key map (kbd "M-S-<tab>") 'buffer-flip-backward)
        (define-key map (kbd "M-ESC")     'buffer-flip-abort)
        map))
;;buffers matching these patterns will be skipped
(setq buffer-flip-skip-patterns
      '("^\\*helm\\b"
        "^\\*Ido\\b"
        "^\\*tramp\\b"
        "^\\*swiper\\*$"))

;;https://github.com/benma/visual-regexp.el
(require 'visual-regexp)

;;https://github.com/jschaf/powershell.el
(require 'powershell)


(provide 'other-setting)

;;; other-setting.el ends here
