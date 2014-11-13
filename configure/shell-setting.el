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
(defun comint-close-completions ()
  "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
  (if comint-dynamic-list-completions-config
      (progn
        (set-window-configuration comint-dynamic-list-completions-config)
        (setq comint-dynamic-list-completions-config nil))))

(defadvice comint-send-input (after close-completions activate)
  (comint-close-completions))

(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (if ad-return-value (comint-close-completions)))

(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (if (member ad-return-value '('sole 'shortest 'partial))
      (comint-close-completions)))

(defadvice comint-dynamic-list-completions (after close-completions activate)
    (comint-close-completions)
    (if (not unread-command-events)
        ;; comint's "Type space to flush" swallows space. put it back in.
        (setq unread-command-events (listify-key-sequence " "))))

(defun kill-shell-buffer(process event)
  "The one actually kill shell buffer when exit. "
  (kill-buffer (process-buffer process))
  ;;(unless (one-window-p) (delete-window))
  )

(defun kill-shell-buffer-after-exit()
  "kill shell buffer when exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-shell-buffer))

(add-hook 'shell-mode-hook 'kill-shell-buffer-after-exit t)

;;start shell prompt
(defun start-shell (buf-name)
  (interactive)
  (switch-to-shell buf-name))

;;popup term
(if-ms-windows    
 (setq popup-terminal-command '("cmd" "/c" "start"))
 (setq popup-terminal-command '("gnome-terminal")))

(defun popup-term ()
  (interactive)
  (apply 'start-process "terminal" nil popup-terminal-command))

;;http://www.emacswiki.org/emacs/multi-shell.el
(require 'multi-shell)
(when-ms-windows
 (setq multi-shell-command "cmdproxy.exe"))

(defun get-local-shell ()
  (interactive)
  (cond
    ;;((string-match "j[ap].*" (getenv "LANG"))
    ;; (with-chinese-env (multi-shell-new)))
    ((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG"))
     (with-chinese-env (multi-shell-new)))
    (t
     (multi-shell-new))
    ))

(defun get-local-curr-shell ()
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
(unless-ms-windows  
 (require 'multi-term)
 (setq multi-term-switch-after-close nil)
 (setq multi-term-dedicated-select-after-open-p t)
 (setq multi-term-program "/bin/bash")
 (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-escape))

 ;;terminator setting
 (require 'terminator)
 (terminator-global-mode t)
 (terminator-basic-setup)
 )

;;set term buffer size to unlimited
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 0)
            (setq show-trailing-whitespace nil)
            ))

;;set key press for term
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y")         'term-paste)
            (define-key term-raw-map (kbd "C-f")         'forward-char)
            (define-key term-raw-map (kbd "C-b")         'backward-char)
            (define-key term-raw-map (kbd "C-a")         'move-beginning-of-line)
            (define-key term-raw-map (kbd "C-e")         'move-end-of-line)
            (define-key term-raw-map (kbd "<down>")      'next-line)
            (define-key term-raw-map (kbd "<up>")        'previous-line)
            (define-key term-raw-map (kbd "<left>")      'backward-char)
            (define-key term-raw-map (kbd "<right>")     'forward-char)
            (define-key term-raw-map (kbd "C-<down>")    'term-send-down)
            (define-key term-raw-map (kbd "C-<up>")      'term-send-up)
            ))

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
        (multi-term-dedicated-toggle))))

;;switch line/char mode
;;Try 'term-line-mode' and 'term-char-mode'
(defun switch-term-and-text ()
  "if current in `term-mode', switch to `text-mode', else switch to `term-mode'."
  (interactive)
  (if (equal major-mode 'term-mode)
      (text-mode)
    (enter-term-mode)))
(defun enter-term-mode ()
  "Enter in `term-mode'."
  (interactive)
  (term-mode)
  (term-char-mode))
(defun enter-text-mode ()
  "Enter in `text-mode'."
  (interactive)
  (text-mode))
  
;;switch to named shell
(setq multi-shell-buffer-name "shell")
(defun my-shell-list ()
  (setq my-shells ())
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
        (setq my-shells (cons  (buffer-name b) my-shells)))))
  (catch 'return
    (throw 'return my-shells)))

;;mulit linux index
(when-ms-windows
 (defvar multi-linux-index 0 "multi shell index")
 (defun get-linux-shell ()
   (interactive)
   (switch-to-shell
    (format "*%s-%d*" multi-shell-buffer-name multi-linux-index))
   (if (>= multi-linux-index 20)
       (setq multi-linux-index 0)
       (setq multi-linux-index (1+ multi-linux-index))))
 )

;; shell on windows
(defvar linux-bash-p t "when open shell on windows using bash flag")

;; switch to shell
(defun switch-to-shell (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Shell name: " (my-shell-list))))
  (if (get-buffer buf-name)
      (progn 
        (switch-to-buffer buf-name))
      (progn
        (if-ms-windows
         (if linux-bash-p
             (progn
              (with-utf-8-env
               (with-start-shell
                (setq shell-file-name "bash")
                (setq shell-command-switch "-c")
                (setq explicit-sh-args '("--login" "-i"))
                (setq explicit-shell-file-name "bash")
                (setenv "SHELL" shell-file-name)
                (shell buf-name))))
             (shell buf-name))
         (shell buf-name))))
  ;;(delete-other-windows)
  (message "switch to %s" buf-name))

;; switch to named term
(defun my-term-list ()
  (setq my-terms ())
  (dolist (b (buffer-list))
    (if (string-match
         (format "^\\\*%s<[0-9]+>\\\*$" multi-term-buffer-name)
         (buffer-name b))
      (progn
        (setq my-terms (cons  (buffer-name b) my-terms)))))
  (catch 'return
    (throw 'return my-terms)))

(defun switch-to-term (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Term name: " (my-term-list))))
  (if (get-buffer buf-name)
      (progn 
        (switch-to-buffer buf-name)
        (message "switch to %s" buf-name)    
        (delete-other-windows))))

;;http://www.docs.uu.se/~mic/emacs.html
(require 'shell-toggle)

(defun clear-input ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

(defun my-comint-hook ()
  (define-key comint-mode-map (kbd "C-c M-o") 'clear-input))
(add-hook 'comint-mode-hook 'my-comint-hook)

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil t)
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m nil t)

;;readline complete
(unless-ms-windows  
 ;;shell completion
 (require 'shell-completion)
 
 (setq explicit-shell-file-name "bash")
 (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
 (setq comint-process-echoes nil)
 
 (require 'readline-complete)
 (add-to-list 'ac-modes 'shell-mode)
 (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
 )

;;eshell setting
(setq eshell-prompt-function
      (lambda ()
        (concat "" (user-login-name) "@" (system-name) " "
                (eshell/pwd) " % ")))

(provide 'shell-setting)

;;; shell-setting.el ends here
