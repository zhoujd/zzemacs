;;;; shell-setting.el --- sample config file
;;;

;;Shell in emacs configure file name by ".emacs_shellname"
;;".emacs_cmdproxy.exe"  ;;for win-7 cmd
;;".emacs_bash"          ;;for bash

;;;For shell ls color on folder
;;export LS_COLORS='di=01;33:ex=92:ln=04;93'
;;alias ls='ls --color=auto'

;;shell settting
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 )

(set-face-foreground 'minibuffer-prompt       "green")
(set-face-foreground 'comint-highlight-prompt "#8ae234")
(set-face-foreground 'comint-highlight-input  "cyan")
(setq shell-font-lock-keywords (list (cons "" 'font-lock-keyword-face)))

;;add shell-scripte-mode support
(setq auto-mode-alist
   (append
    (list (cons "profile$"         'shell-script-mode))
    (list (cons "\\.bash_profile$" 'shell-script-mode))
    (list (cons "\\.bashrc$"       'shell-script-mode))
    auto-mode-alist))

;;interpret and use ansi color codes in shell output windows
(when (fboundp 'ansi-color-for-comint-mode-on)
  ;;escape sequence
  (autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;;automatically_close_completions_in_emacs_shell_comint_mode.txt
(defun zz:comint-close-completions ()
  "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
  (if comint-dynamic-list-completions-config
      (progn
        (set-window-configuration comint-dynamic-list-completions-config)
        (setq comint-dynamic-list-completions-config nil))))

(defadvice comint-send-input (after close-completions activate)
  (zz:comint-close-completions))

(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (if ad-return-value (zz:comint-close-completions)))

(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (if (member ad-return-value '('sole 'shortest 'partial))
      (zz:comint-close-completions)))

(defadvice comint-dynamic-list-completions (after close-completions activate)
    (zz:comint-close-completions)
    (if (not unread-command-events)
        ;; comint's "Type space to flush" swallows space. put it back in.
        (setq unread-command-events (listify-key-sequence " "))))

(defun zz:kill-shell-buffer(process event)
  "The one actually kill shell buffer when exit. "
  (kill-buffer (process-buffer process)))

(defun zz:kill-shell-buffer-after-exit()
  "kill shell buffer when exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'zz:kill-shell-buffer))

(add-hook 'shell-mode-hook 'zz:kill-shell-buffer-after-exit t)

;;start shell prompt
(defun zz:start-shell (buf-name)
  (interactive)
  (zz:switch-to-shell buf-name))

;;popup term
(if-ms-windows
 (setq popup-terminal-command '("cmd" "/c" "start"))
 (setq popup-terminal-command '("urxvt")))

(defun zz:popup-term ()
  (interactive)
  (apply 'start-process "terminal" nil popup-terminal-command))

;;http://www.emacswiki.org/emacs/multi-shell.el
(require 'multi-shell)
(when-ms-windows
 (setq multi-shell-command "cmdproxy"))

(defun zz:get-local-shell ()
  (interactive)
  (cond
    ;;((string-match "j[ap].*" (getenv "LANG"))
    ;; (with-chinese-env (multi-shell-new)))
    ((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG"))
     (with-chinese-env (multi-shell-new)))
    (t
     (multi-shell-new))
    ))

(defun zz:get-local-curr-shell ()
  (interactive)
  (cond
    ;;((string-match "j[ap].*" (getenv "LANG"))
    ;; (with-chinese-env (multi-shell-current-directory)))
    ((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG"))
     (with-chinese-env (multi-shell-current-directory)))
    (t
     (multi-shell-current-directory))
    ))

;;http://www.emacswiki.org/emacs/MultiTerm
;;http://code.google.com/p/dea/source/browse/trunk/my-lisps/multi-term-settings.el
;;switch line/char mode
;;C-c C-j => 'term-line-mode'
;;C-c C-k => 'term-char-mode'
(unless-ms-windows
 (require 'multi-term)
 (setq multi-term-switch-after-close nil)
 (setq multi-term-dedicated-select-after-open-p t)
 (setq multi-term-scroll-to-bottom-on-output t)
 (setq multi-term-program "bash")
 (setq multi-term-program-switches "--login")
 
 (set-terminal-coding-system 'utf-8-unix)
 (add-hook 'term-exec-hook
           (function
            (lambda ()
              (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

 ;;tmux prefix
 (defun zz:term-send-tmux ()
   "Use term-send-raw-string \"\C-b\" for tmux"
  (interactive)
  (term-send-raw-string "\C-b"))
 (add-to-list 'term-bind-key-alist '("C-c C-b" . zz:term-send-tmux))

 ;;screen prefix
 (defun zz:term-send-screen ()
   "Use term-send-raw-string \"\C-a\" for screen"
  (interactive)
  (term-send-raw-string "\C-a"))
 (add-to-list 'term-bind-key-alist '("C-c C-a" . zz:term-send-screen))

 ;;terminator setting
 (require 'terminator)
 (terminator-global-mode t)
 (terminator-basic-setup)
 (defkeys-map terminator-mode-map
  ((kbd "C-c t 1") (zz:quick-termintor zz:terminator-0 0))
  ((kbd "C-c t 2") (zz:quick-termintor zz:terminator-1 1))
  ((kbd "C-c t 3") (zz:quick-termintor zz:terminator-2 2))
  ((kbd "C-c t 4") (zz:quick-termintor zz:terminator-3 3))
  ))


;;set term buffer size to unlimited
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 0)
            (setq show-trailing-whitespace nil)
            ))

(eval-after-load "term"
  '(progn
     ;; ensure that scrolling doesn't break on output
     (setq term-scroll-to-bottom-on-output t)))

(defun zz:term-send-clear ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\C-l"))

(defun zz:kill-ring-save-switch-to-char-mode (b e)
  "In line-mode, M-w also switches back to char-mode and goes back to prompt."
  (interactive "r")
  (kill-ring-save b e t)
  (when (term-in-line-mode)
    (term-char-mode)
    (term-send-raw-string "")))

;;key set for term
(add-hook 'term-mode-hook
          (lambda ()
            (defkeys-map term-raw-map
              ((kbd "C-c C-j") 'term-line-mode)
              ((kbd "C-c C-k") 'term-char-mode)
              ((kbd "C-c C-q") 'term-pager-toggle)
              
              ((kbd "C-c [")   'term-line-mode)
              ((kbd "C-c ]")   'term-char-mode)
              ((kbd "C-c M-o") 'zz:term-send-clear)
              ((kbd "M-w")     'zz:kill-ring-save-switch-to-char-mode))
            
            (defkeys-map term-mode-map
              ((kbd "C-c [")   'term-line-mode)
              ((kbd "C-c ]")   'term-char-mode)
              ((kbd "C-c M-o") 'zz:term-send-clear)
              ((kbd "M-w")     'zz:kill-ring-save-switch-to-char-mode)
              )))

(defun zz:last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (zz:last-term-buffer (cdr l)))))

(defun zz:get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (zz:last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
        (switch-to-buffer b))))

(defun zz:multi-term-dedicated-toggle ()
  "jump back to previous location after toggling ded term off"
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (progn
        (multi-term-dedicated-toggle)
        (switch-to-buffer-other-window old-buf))
      (progn
        (setq old-buf (current-buffer))
        (multi-term-dedicated-toggle))))

;;switch to named shell
(setq multi-shell-buffer-name "shell")
(defun zz:shell-list ()
  (setq zz:shells ())
  (dolist (b (buffer-list))
    (if (or (string-match
             (format "^\\\*%s\\\*$" "shell")
             (buffer-name b))
            (string-match
             (format "^\\\*%s-[a-zA-Z0-9]+\\\*$" multi-shell-buffer-name)
             (buffer-name b))
            (string-match
             (format "^\\\*%s<[0-9]+>\\\*$" multi-shell-buffer-name)
             (buffer-name b)))
      (progn
        (setq zz:shells (cons  (buffer-name b) zz:shells)))))
  (catch 'return
    (throw 'return zz:shells)))

;;mulit linux index
(when-ms-windows
 (defvar multi-linux-index 0 "multi shell index")
 (defvar multi-linux-max 20 "multi shell max index")
 (defvar multi-linux-name multi-shell-buffer-name "multi shell index")
 (defun zz:get-linux-shell ()
   (interactive)
   (zz:switch-to-shell
    (format "*%s-%d*" multi-linux-name multi-linux-index))
   (if (>= multi-linux-index multi-linux-max)
       (setq multi-linux-index 0)
       (setq multi-linux-index (1+ multi-linux-index))
     )))

;; shell on windows
(defvar zz:linux-bash-p t "when open shell on windows using bash flag")

;; default login_shelll for linux
(unless-ms-windows
 (setq shell-file-name "bash")
 (setq shell-command-switch "-c")
 (setq explicit-bash-args '("--login" "-i"))
 (setq explicit-shell-file-name "bash"))

;; create shell buffer
(defun zz:create-shell-buffer (buf-name)
  (save-window-excursion
   (shell buf-name))
  (switch-to-buffer buf-name))

;; switch to shell
(defun zz:switch-to-shell (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Shell name: " (zz:shell-list))))
  (if (get-buffer buf-name)
      (progn
        (switch-to-buffer buf-name))
      (progn
        (if-ms-windows
         (if zz:linux-bash-p
             (progn
              (with-utf-8-env
               (let ((shell-file-name "bash")
                     (shell-command-switch "-c")
                     (explicit-bash-args '("--login" "-i"))
                     (explicit-shell-file-name "bash"))
                 (setenv "SHELL" shell-file-name)
                 (zz:create-shell-buffer buf-name)
                 )))
             (progn
              (zz:create-shell-buffer buf-name)))
         (progn
          (zz:create-shell-buffer buf-name))
          )))
  (message "switch to %s" buf-name))

;; switch to named term
(defun zz:term-list ()
  (setq zz:terms ())
  (dolist (b (buffer-list))
    (if (string-match
         (format "^\\\*%s<[0-9]+>\\\*$" multi-term-buffer-name)
         (buffer-name b))
      (progn
        (setq zz:terms (cons  (buffer-name b) zz:terms)))))
  (catch 'return
    (throw 'return zz:terms)))

(defun zz:switch-to-term (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Term name: " (zz:term-list))))
  (if (get-buffer buf-name)
      (progn
        (switch-to-buffer buf-name)
        (message "switch to %s" buf-name)
        (delete-other-windows))))

;;http://www.docs.uu.se/~mic/emacs.html
(require 'shell-toggle)

(defun zz:clear-input ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

(defun zz:comint-hook ()
  (defkeys-map comint-mode-map
    ((kbd "C-c M-o") 'zz:clear-input)))
(add-hook 'comint-mode-hook 'zz:comint-hook)

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil t)
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m nil t)

;;shell completion
(unless-ms-windows
 (require 'shell-completion))

;;https://github.com/szermatt/emacs-bash-completion
(unless-ms-windows
 (require 'bash-completion)
 (bash-completion-setup))

;;eshell setting
(setq eshell-prompt-function
      (lambda ()
        (concat "" (user-login-name) "@" (system-name) " "
                (eshell/pwd) " % ")))

;;M-x cd /hostname:/current/path/in/the/shell
(defun zz:remote-shell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/" tramp-default-method ":" host ":"))
      (zz:get-local-shell)
      )))

(defun zz:local-shell (&optional dir)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((dir (if dir dir (read-string "Dir: "))))
      (cd dir)
      (zz:get-local-shell)
      )))

(defun zz:shell-directory (name dir)
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell name)))

;;xterm color
(require 'xterm-color)
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))
(add-hook 'shell-mode-hook
          (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;;eterm-256color
;;eterm-color: apt install ncurses-term
(require 'eterm-256color)
(add-hook 'term-mode-hook #'eterm-256color-mode)


(provide 'shell-setting)

;;; shell-setting.el ends here
