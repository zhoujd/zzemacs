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
 '(comint-prompt-read-only t)           ; if this is t, it breaks shell-command
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

;;disable undo warning
(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

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

;;http://www.emacswiki.org/emacs/multi-shell.el
(require 'multi-shell)
(when-ms-windows
 (setq multi-shell-command "cmdproxy"))

(defun zz:get-shell ()
  (interactive)
  (cond
    ;;((string-match "j[ap].*" (getenv "LANG"))
    ;; (with-chinese-env (multi-shell-new)))
    ((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG"))
     (with-chinese-env (multi-shell-new)))
    (t
     (multi-shell-new))
    ))

(defun zz:get-current-shell ()
  (interactive)
  (cond
    ;;((string-match "j[ap].*" (getenv "LANG"))
    ;; (with-chinese-env (multi-shell-current-directory)))
    ((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG"))
     (with-chinese-env (multi-shell-current-directory)))
    (t
     (multi-shell-current-directory))
    ))

;;switch to named shell
(setq multi-shell-buffer-name "shell")
(defun zz:shell-list ()
  (setq zz:shells ())
  (dolist (b (buffer-list))
    (if (or (string-match
             (format "^\\\*%s\\\*$" multi-shell-buffer-name)
             (buffer-name b))
            (string-match
             (format "^\\\*%s-[a-zA-Z0-9]+\\\*$" multi-shell-buffer-name)
             (buffer-name b))
            (string-match
             (format "^\\\*%s<f[0-9]+>\\\*$" multi-shell-buffer-name)
             (buffer-name b))
            (string-match
             (format "^\\\*%s<[0-9]+>\\\*$" multi-shell-buffer-name)
             (buffer-name b)))
      (progn
        (setq zz:shells (cons  (buffer-name b) zz:shells)))))
  (catch 'return
    (throw 'return zz:shells)))

(defun zz:shell-c-list ()
  (setq zz:c-shells ())
  (dolist (b (buffer-list))
    (if (string-match
         (format "^\\\*%s<[0-9]+>\\\*$" multi-shell-buffer-name)
         (buffer-name b))
        (progn
          (setq zz:c-shells (cons  (buffer-name b) zz:c-shells)))))
  (catch 'return
    (throw 'return zz:c-shells)))

(defun zz:shell-fn-list ()
  (setq zz:fn-shells ())
  (dolist (b (buffer-list))
    (if (string-match
         (format "^\\\*%s<f[0-9]+>\\\*$" multi-shell-buffer-name)
         (buffer-name b))
        (progn
          (setq zz:fn-shells (cons  (buffer-name b) zz:fn-shells)))))
  (catch 'return
    (throw 'return zz:fn-shells)))

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

;; switch to c shell
(defun zz:switch-to-c-shell (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Shell name: " (zz:shell-c-list))))
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

;; switch to fn shell
(defun zz:switch-to-fn-shell (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Shell name: " (zz:shell-fn-list))))
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
  "Open a remote shell to a host"
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/" tramp-default-method ":" host ":"))
      (zz:get-shell)
      )))

(defun zz:cd-shell ()
  "Open a cd shell"
  (interactive)
  (let ((dir (file-name-as-directory
              (ido-read-directory-name "Directory: "))))
    (with-temp-buffer
      (cd dir)
      (zz:get-shell)
      )))

(defun zz:shell-directory (name dir)
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell name)))

;;shell mode
(require 'company-shell)

;;shell-mode use company-mode
(add-hook 'shell-mode-hook
          (lambda ()
            (company-mode t)
            (define-key shell-mode-map (kbd "TAB") #'company-manual-begin)))


(provide 'shell-setting)

;;; shell-setting.el ends here
