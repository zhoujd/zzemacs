;;;; term-setting.el --- sample config file
;;;https://www.emacswiki.org/emacs/AnsiTermHints

;;tramp-term
(require 'tramp-term)

(zz:load-path "site-lisp/prf-tramp")
(require 'prf-tramp-friendly-parsing)

;;open localhost ansi-term
(defun zz:get-local-term ()
  (interactive)
  (ansi-term "bash" "localhost"))

;;popup term
(if-ms-windows
 (setq popup-terminal-command '("cmd" "/c" "start"))
 (setq popup-terminal-command '("urxvt")))

(defun zz:popup-term ()
  (interactive)
  (apply 'start-process "terminal" nil popup-terminal-command))

;;http://www.emacswiki.org/emacs/MultiTerm
;;http://code.google.com/p/dea/source/browse/trunk/my-lisps/multi-term-settings.el
;;switch line/char mode
;;C-c C-j => 'term-line-mode'
;;C-c C-k => 'term-char-mode'
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
  ((kbd "C-c t 4") (zz:quick-termintor zz:terminator-3 3)))

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
              ((kbd "C-h")     'term-send-backspace)
              ((kbd "M-w")     'zz:kill-ring-save-switch-to-char-mode))

            (defkeys-map term-mode-map
              ((kbd "C-c [")   'term-line-mode)
              ((kbd "C-c ]")   'term-char-mode)
              ((kbd "C-c M-o") 'zz:term-send-clear)
              ((kbd "C-h")     'term-send-backspace)
              ((kbd "M-w")     'zz:kill-ring-save-switch-to-char-mode)
              )))

(defun zz:last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (zz:last-term-buffer (cdr l)))))

(defun zz:open-term-dir (dir)
  (let ((multi-term-default-dir dir))
    (multi-term)
    (when (tramp-tramp-file-p dir)
      (tramp-term--initialize (prf/tramp/get-host-from-path dir)))))

(defun zz:open-term-host (host)
    (multi-term)
    (tramp-term--initialize host))

(defun zz:get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (zz:last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (zz:open-term-dir default-directory)
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

;;xterm color
(require 'xterm-color)
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))
(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;;eterm-256color
;;eterm-color: apt install ncurses-term
(require 'eterm-256color)
(add-hook 'term-mode-hook #'eterm-256color-mode)

;;remote ssh
(defun zz:ssh (host port)
  "Connect to a remote host by SSH."
  (interactive "sHost: \nsPort (default 22): ")
  (let* ((port (if (equal port "") "22" port))
         (switches (list host "-p" port))
         (buf (format "ssh:%s" host)))
    (set-buffer (apply 'make-term buf "ssh" nil switches))
    (term-mode)
    (term-char-mode)
    (tramp-term--initialize host)
    (switch-to-buffer (format "*%s*" buf))
    ))

(defun zz:cd-term ()
  (interactive)
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory
                               (ido-read-directory-name "Directory: "))))
      (zz:open-term-dir default-directory))))

(defun zz:home-term ()
  (interactive)
  (with-temp-buffer
    (let* ((default-directory "~"))
      (zz:open-term-dir default-directory))))

(defun zz:local-term ()
  (interactive)
  (with-temp-buffer
    (when (tramp-tramp-file-p default-directory)
      (setq default-directory "~"))
    (let* ((default-directory (file-name-as-directory
                               (ido-read-directory-name "Directory: "))))
      (zz:open-term-dir default-directory))))

(defun zz:get-remote-term (host)
  "Connect to a remote host by multi-term."
  (with-temp-buffer
    (let* ((multi-term-program "ssh")
           (multi-term-program-switches (format "%s" host))
           (default-directory "~"))
      (zz:open-term-host host)
      (message "Remote %s ready" host)
      )))

(defun zz:remote-term (host)
  "Connect to a remote host by multi-term."
  (interactive "sHost: ")
  (zz:get-remote-term host))

(defun zz:helm-cd-term (dir)
  (interactive "DDirectory: ")
  (with-temp-buffer
    (let* ((default-directory dir))
      (zz:open-term-dir default-directory))))

(defun zz:helm-local-term ()
  "remote term with helm"
  (interactive)
  (with-temp-buffer
    (let* ((prefix "~"))
      (when (tramp-tramp-file-p default-directory)
        (setq default-directory prefix))
      (call-interactively 'zz:helm-cd-term)
    )))

(defun zz:helm-remote-term ()
  "remote term with helm"
  (interactive)
  (let ((host (car (tramp-term--select-host))))
    (zz:get-remote-term host)))

;;auto kill term buffer
(add-hook 'term-exec-hook (lambda ()
            (let* ((buff (current-buffer))
                 (proc (get-buffer-process buff)))
            (lexical-let ((buff buff))
               (set-process-sentinel proc (lambda (process event)
                            (if (string= event "finished\n")
                                       (kill-buffer buff))))))))


(provide 'term-setting)

;;; term-setting.el ends here
