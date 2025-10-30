;;;; shell-setting.el --- sample config file
;;;

;;Shell in emacs configure file name by ".emacs_shellname"
;;".emacs_cmdproxy.exe"  ;;for win-7 cmd
;;".emacs_bash"          ;;for bash

;;;For shell ls color on folder
;;export LS_COLORS='di=01;33:ex=92:ln=04;93'
;;alias ls='ls --color=auto'

;;;Modify indent for shell mode
;;M-x eval-expression
;;(setq sh-basic-offset 2)
;;(setq-default sh-indent-for-case-label 0)
;;(setq-default sh-indent-for-case-alt '+)

(require 'comint)

;;shell settting
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command and gud-print
 )

;;;face color on gruvbox theme
;;(set-face-foreground 'minibuffer-prompt       "#79740e")
;;(set-face-foreground 'comint-highlight-prompt "#afaf00")
;;(set-face-foreground 'comint-highlight-input  "#949494")
(setq shell-font-lock-keywords (list (cons "" 'font-lock-keyword-face)))

;;shell prompt color
(defun zz/shell-prompt ()
  "Color prompt on shell-mode to use PS1"
  (face-remap-set-base 'comint-highlight-prompt :inherit nil))
(add-hook 'shell-mode-hook 'zz/shell-prompt)

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

;;use ansi color codes in shell output windows
(when (fboundp 'ansi-color-for-comint-mode-on)
  ;;If you can't stop ANSI escape sequences, use them
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;;xterm color
(require 'xterm-color)
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))
(defun zz/xterm-color ()
  "Color on shell-mode output support"
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))
(add-hook 'shell-mode-hook 'zz/xterm-color)

;;automatically_close_completions_in_emacs_shell_comint_mode.txt
(defun zz/comint-close-completions ()
  "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
  (if comint-dynamic-list-completions-config
      (progn
        (set-window-configuration comint-dynamic-list-completions-config)
        (setq comint-dynamic-list-completions-config nil))))

(defadvice comint-send-input (after close-completions activate)
  (zz/comint-close-completions))

(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (if ad-return-value (zz/comint-close-completions)))

(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (if (member ad-return-value '('sole 'shortest 'partial))
      (zz/comint-close-completions)))

(defadvice comint-dynamic-list-completions (after close-completions activate)
    (zz/comint-close-completions)
    (if (not unread-command-events)
        ;; comint's "Type space to flush" swallows space. put it back in.
        (setq unread-command-events (listify-key-sequence " "))))

(defun zz/kill-shell-buffer(process event)
  "The one actually kill shell buffer when exit. "
  (kill-buffer (process-buffer process)))

(defun zz/kill-shell-buffer-after-exit()
  "kill shell buffer when exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'zz/kill-shell-buffer))

(add-hook 'shell-mode-hook 'zz/kill-shell-buffer-after-exit t)

;;start shell prompt
(defun zz/start-shell (buf-name)
  (interactive)
  (zz/switch-to-shell buf-name)
  (when-ms-windows
   (zz/clear-input)))

;;http://www.emacswiki.org/emacs/multi-shell.el
(require 'multi-shell)
(when-ms-windows
 (setq multi-shell-command "cmdproxy"))

(defun zz/get-shell ()
  "Get shell"
  (interactive)
  (let ((default-directory (file-name-as-directory
                            (ido-read-directory-name "Directory: "))))
    (when (file-exists-p default-directory)
      (multi-shell-new))))

(defun zz/get-sh ()
  "Get /bin/sh shell"
  (interactive)
  (let ((default-directory (file-name-as-directory
                            (ido-read-directory-name "Directory: ")))
        (multi-shell-command "/bin/sh"))
    (when (file-exists-p default-directory)
      (multi-shell-new))))

(defun zz/home-shell ()
  (interactive)
  (let ((default-directory "~"))
    (when (file-exists-p default-directory)
      (multi-shell-new))))

(defun zz/get-current-shell ()
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
(defun zz/shell-list ()
  (setq zz/shells ())
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
        (setq zz/shells (cons  (buffer-name b) zz/shells)))))
  (catch 'return
    (throw 'return zz/shells)))

(defun zz/shell-c-list ()
  (setq zz/c-shells ())
  (dolist (b (buffer-list))
    (if (string-match
         (format "^\\\*%s<[0-9]+>\\\*$" multi-shell-buffer-name)
         (buffer-name b))
        (progn
          (setq zz/c-shells (cons  (buffer-name b) zz/c-shells)))))
  (catch 'return
    (throw 'return zz/c-shells)))

(defun zz/shell-fn-list ()
  (setq zz/fn-shells ())
  (dolist (b (buffer-list))
    (if (string-match
         (format "^\\\*%s<f[0-9]+>\\\*$" multi-shell-buffer-name)
         (buffer-name b))
        (progn
          (setq zz/fn-shells (cons  (buffer-name b) zz/fn-shells)))))
  (catch 'return
    (throw 'return zz/fn-shells)))

;;mulit linux index
(when-ms-windows
 (defvar multi-linux-index 0 "multi shell index")
 (defvar multi-linux-max 20 "multi shell max index")
 (defvar multi-linux-name multi-shell-buffer-name "multi shell index")
 (defun zz/get-linux-shell ()
   (interactive)
   (zz/switch-to-shell
    (format "*%s-%d*" multi-linux-name multi-linux-index))
   (if (>= multi-linux-index multi-linux-max)
       (setq multi-linux-index 0)
       (setq multi-linux-index (1+ multi-linux-index))
     )))

;; shell on windows
(defvar zz/linux-bash-p t "when open shell on windows using bash flag")

;; default login_shelll for linux
(unless-ms-windows
 (setq shell-file-name "bash")
 (setq shell-command-switch "-c")
 (setq explicit-bash-args '("--login" "-i"))
 (setq explicit-shell-file-name "bash"))

;; create shell buffer
(defun zz/create-shell-buffer (buf-name)
  (save-window-excursion
   (shell buf-name))
  (switch-to-buffer buf-name))

;; switch to shell
(defun zz/switch-to-shell (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Shell name: " (zz/shell-list))))
  (if (get-buffer buf-name)
      (progn
        (switch-to-buffer buf-name))
      (progn
        (if-ms-windows
         (if zz/linux-bash-p
             (progn
              (with-utf-8-env
               (let ((shell-file-name "bash")
                     (shell-command-switch "-c")
                     (explicit-bash-args '("--login" "-i"))
                     (explicit-shell-file-name "bash"))
                 (setenv "SHELL" shell-file-name)
                 (zz/create-shell-buffer buf-name)
                 )))
             (progn
              (zz/create-shell-buffer buf-name)))
         (progn
          (zz/create-shell-buffer buf-name))
          )))
  (message "switch to %s" buf-name))

;; switch to c shell
(defun zz/switch-to-c-shell (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Shell name: " (zz/shell-c-list))))
  (if (get-buffer buf-name)
      (progn
        (switch-to-buffer buf-name))
      (progn
        (if-ms-windows
         (if zz/linux-bash-p
             (progn
              (with-utf-8-env
               (let ((shell-file-name "bash")
                     (shell-command-switch "-c")
                     (explicit-bash-args '("--login" "-i"))
                     (explicit-shell-file-name "bash"))
                 (setenv "SHELL" shell-file-name)
                 (zz/create-shell-buffer buf-name)
                 )))
             (progn
              (zz/create-shell-buffer buf-name)))
         (progn
          (zz/create-shell-buffer buf-name))
          )))
  (message "switch to %s" buf-name))

;; switch to fn shell
(defun zz/switch-to-fn-shell (buf-name)
  "switch to named shell buffer it not exist creat it by name"
  (interactive (list (ido-completing-read "Shell name: " (zz/shell-fn-list))))
  (if (get-buffer buf-name)
      (progn
        (switch-to-buffer buf-name))
      (progn
        (if-ms-windows
         (if zz/linux-bash-p
             (progn
              (with-utf-8-env
               (let ((shell-file-name "bash")
                     (shell-command-switch "-c")
                     (explicit-bash-args '("--login" "-i"))
                     (explicit-shell-file-name "bash"))
                 (setenv "SHELL" shell-file-name)
                 (zz/create-shell-buffer buf-name)
                 )))
             (progn
              (zz/create-shell-buffer buf-name)))
         (progn
          (zz/create-shell-buffer buf-name))
          )))
  (message "switch to %s" buf-name))

;;http://www.docs.uu.se/~mic/emacs.html
(require 'shell-toggle)

(defun zz/clear-input ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

(defun zz/comint-hook ()
  (defkeys-map comint-mode-map
    ((kbd "C-c M-o") 'zz/clear-input)))
(add-hook 'comint-mode-hook 'zz/comint-hook)

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

(defun zz/cd ()
  "cd with ido"
  (interactive)
  (let ((dir (file-name-as-directory
              (ido-read-directory-name "Directory: "))))
    (when (file-exists-p dir)
      (cd dir))))

(defun zz/cd-shell ()
  "Open a cd shell"
  (interactive)
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory
                               (ido-read-directory-name "Directory: "))))
      (when (file-exists-p default-directory)
        (multi-shell-new)
        ))))

(defun zz/local-shell ()
  "Open a local shell"
  (interactive)
  (with-temp-buffer
    (let* ((prefix "~"))
      (when (tramp-tramp-file-p default-directory)
        (setq default-directory prefix))
      (call-interactively 'zz/get-shell)
      )))

(defun zz/local-sh ()
  "Open a local /bin/sh"
  (interactive)
  (with-temp-buffer
    (let* ((prefix "~"))
      (when (tramp-tramp-file-p default-directory)
        (setq default-directory prefix))
      (call-interactively 'zz/get-sh)
      )))

(defun zz/get-host ()
  (with-temp-buffer
    (let* ((default-directory "~")
           (cat "cat ~/.ssh/config ~/.ssh/config.d/* 2>&-")
           (grep "grep -i -e '^host ' | grep -v '[*?]' | grep -v 'git'")
           (awk "awk '/^Host/{if (NR!=1)print \"\"; printf $2}'")
           (cmd (format "%s | %s | %s" cat grep awk))
           (host (ido-completing-read "Host: "
                                      (split-string
                                       (shell-command-to-string cmd)))))
      (if (eq tramp-syntax 'simplified)
          (concat "/" host ":")
          (concat "/" tramp-default-method ":" host ":"))
      )))

(defun zz/remote-shell ()
  "Open a remote shell to a host"
  (interactive)
  (with-temp-buffer
    (let* ((default-directory (zz/get-host)))
      (when (file-exists-p default-directory)
        (call-interactively 'zz/get-shell))
      )))

(defun zz/remote-sh ()
  "Open a remote /bin/sh to a host"
  (interactive)
  (with-temp-buffer
    (let* ((default-directory (zz/get-host)))
      (when (file-exists-p default-directory)
        (call-interactively 'zz/get-sh))
      )))

(defun zz/helm-cd (dir)
  (interactive "DDirectory: ")
  (when (file-exists-pexists-p dir)
    (cd dir)))

(defun zz/helm-cd-shell (dir)
  (interactive "DDirectory: ")
  (with-temp-buffer
    (let ((default-directory dir))
      (when (file-exists-p default-directory)
        (multi-shell-new)))))

(defun zz/helm-local-shell ()
  "local shell with helm"
  (interactive)
  (with-temp-buffer
    (let* ((prefix "~"))
      (when (tramp-tramp-file-p default-directory)
        (setq default-directory prefix))
      (call-interactively 'zz/helm-cd-shell)
      )))

(defun zz/helm-get-remote ()
  (with-temp-buffer
    (let* ((path "~/.ssh/config ~/.ssh/config.d/* 2>/dev/null")
           (grep "grep -i -e '^host ' | grep -v '[*?]' | grep -v 'git.*com'")
           (awk "awk '/^Host/{if (NR!=1)print \"\"; printf $2}'")
           (command (format "cat %s | %s | %s" path grep awk))
           (host (helm-comp-read "Host: "
                                 (split-string
                                  (shell-command-to-string command)))))
      (if (eq tramp-syntax 'simplified)
          (concat "/" host ":")
          (concat "/" tramp-default-method ":" host ":")))
    ))

(defun zz/helm-get-host ()
  (let ((default-directory "~/.ssh"))
    (zz/helm-get-remote)))

(defun zz/helm-remote-shell ()
  "remote shell with helm"
  (interactive)
  (with-temp-buffer
    (let* ((default-directory (zz/helm-get-host)))
      (when (file-exists-p default-directory)
        (call-interactively 'zz/helm-cd-shell))
      )))

(defun zz/shell-directory (name dir)
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell name)))

;;trans shell
(defun zz/trans-shell ()
  (interactive)
  (let ((shell-file-name "bash")
        (shell-command-switch "-c")
        (explicit-bash-args '("trans" "-I" "-no-rlwrap"))
        (explicit-shell-file-name "bash"))
    (if (executable-find "trans")
        (zz/create-shell-buffer "*trans shell*")
        (message "Cannot find trans!"))
    ))

;;custom shell indent
(defun zz/shell-indent (num)
  (interactive "nIndent: ")
  (eval-expression
   '(progn
      (setq sh-basic-offset num)))
  (message "Select and press TAB to indent: %d" num))

;;shell-script-mode
(defun zz/sh-mode-hook ()
  (defkeys-map sh-mode-map
    ((kbd "C-c M-/")    'zz/company-shell)))
(add-hook 'sh-mode-hook 'zz/sh-mode-hook)

;;shell-mode use company-mode
(defun zz/comint-send-input()
  (interactive)
  (company-abort)
  (comint-send-input))
(defun zz/comint-send-select()
  (interactive)
  (company-complete-selection)
  (comint-send-input))
(defun zz/shell-company-hook ()
  (company-mode t)
  (defkeys-map shell-mode-map
    ((kbd "TAB")        'company-manual-begin)
    ((kbd "RET")        'zz/comint-send-input)
    ((kbd "<C-return>") 'zz/comint-send-select)
    ((kbd "C-c M-/")    'zz/company-shell)))
(add-hook 'shell-mode-hook 'zz/shell-company-hook)

;;shell-mode common
(defun zz/shell-common-hook ()
  (setq-local comint-prompt-read-only t))   ;;readonly prompt
(add-hook 'shell-mode-hook 'zz/shell-common-hook)

;;https://repo.or.cz/emacs-coterm.git
;(require 'coterm)
;(coterm-mode t)


(provide 'shell-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; shell-setting.el ends here
