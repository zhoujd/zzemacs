;;;; common-setting.el --- common config file
;;

;;tell me if there's something wrong
;(setq debug-on-error t)

(zz-load-path "site-lisp")

;;disable loading of "default.el" at startup
;(setq inhibit-default-init t)
;(keyboard-translate ?\C-h ?\C-?)  ; translate `C-h' to DEL
;(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'.

;;the current frame to make it transparent
;(set-frame-parameter (selected-frame) 'alpha '(95 70))
;(add-to-list 'default-frame-alist '(alpha 95 70))

;; -*- Chinese -*-
(defun my-set-language-chinese ()
  "This is for chinese setting"
  (interactive)
  (set-language-environment 'Chinese-GB18030)
  (set-buffer-file-coding-system 'chinese-gb18030)
  (message "This is for chinese"))

;; -*- Japanese -*-
(defun my-set-language-japanese ()
  "This is for japanese setting"
  (interactive)
  (set-language-environment 'Japanese)
  (set-buffer-file-coding-system 'japanese-shift-jis)
  (message "This is for japanese"))

;; -*- utf-8 -*-
(defun my-set-language-utf-8 ()
  "This is for utf-8 setting"
  (interactive)
  (set-language-environment 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (message "This is for utf-8"))

;; -*- Language switch -*-
(cond
  ((string-match "j[ap].*" (getenv "LANG"))
   (my-set-language-japanese))
  ((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG"))
   (my-set-language-chinese))
  (t (my-set-language-utf-8)))

;;font setting
(setq en-font-list '("Consolas 11" "Inconsolata 12" "Monaco 10" "DejaVu Sans Mono 12"))
(setq cn-font-list '("Microsoft Yahei 13" "Microsoft YaHei Mono 14" "文泉驿等宽微米黑 14" "新宋体 14")) 

(setq my-font-en-name (nth 0 en-font-list))
(setq my-font-cn-name (nth 1 cn-font-list))

(defun my-cn-font-name ()
  (string-match ".*[ ]" my-font-cn-name)  
  (setq my-cn-name (substring (match-string 0 my-font-cn-name) 0 -1)))

(defun my-cn-font-size ()
  (string-to-number (car (last (split-string my-font-cn-name)))))

(defun my-frame-font ()
  "my frame font setting"
  ;; Setting English Font
  (set-face-attribute
   'default nil :font my-font-en-name)
  ;; Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family (my-cn-font-name)
                                 :size   (my-cn-font-size)))))

(setq my-font-console-name (nth 1 cn-font-list))
(defun my-console-font ()
  "my console font setting"
  (progn
    (add-to-list 'default-frame-alist
                 '(font . my-font-console-name))))

(if window-system (my-frame-font) (my-console-font))

;;server-mode
;;emacsclientw.exe -f "~\.emacs.d\server\server" -n -a "runemacs.exe" path\to\file
;;emacsclientw.exe --server-file ~\.emacs.d\server\server -n -a runemacs.exe path\to\file
;;~/.emacs.d/server的属主由Administrators组改为当前用户（右键属性--安全--高级--所有者）
(require 'server)
(when (and (>= emacs-major-version 23)
           (or (eq window-system 'w32) (eq window-system 'win32)))
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
					                             ; ~/.emacs.d/server is unsafe"
					                             ; on windows.
(unless (server-running-p)
  (server-start))

(add-hook 'kill-emacs-hook
	  (lambda()
	    (if (file-exists-p "~/.emacs.d/server/server")
		(delete-file "~/.emacs.d/server/server"))))

;;color theme
(zz-load-path "site-lisp/color-theme")
(require 'color-theme)
(require 'color-theme-blackboard)
(require 'color-theme-tango)
(color-theme-initialize)

(setq color-theme-choices '(color-theme-gnome2
                            color-theme-blackboard
                            color-theme-blackboard2
                            color-theme-tango-dark
                            ))

(if window-system
    (progn
      (funcall (nth 3 color-theme-choices))
      ;(funcall (nth (random (length color-theme-choices)) color-theme-choices))
      )
    (progn
      (set-face-background 'default "black")
      (set-face-foreground 'default "gray")))

;;set default-frame-alist
(if window-system
    (setq default-frame-alist
          (append
           '((scroll-bar-width . 16)
             (width . 140)
             (height . 40))
           default-frame-alist)))

;;quick display key help
(setq echo-keystrokes 0.1)

;;scroll bar right
(set-scroll-bar-mode `right)

;;no need temp file
(setq make-backup-files nil)
(setq-default make-backup-files nil)
;;no file #filename#
(setq auto-save-default nil) 

;;auto add newline in file
(setq require-final-newline t)
;;keep cursor on tail of line
;(setq track-eol t)
;;keep slience
(setq visible-bell t);
;;don`t flash the screen on console mode
;(setq ring-bell-function 'ignore)
(setq ring-bell-function (lambda ()  t))
;;use clipboard
(setq x-select-enable-clipboard t)
;;mouse select
(setq mouse-drag-copy-region nil)
;;stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-primary nil) 
;;active region sets primary X11 selection
(setq select-active-regions t)

(global-auto-revert-mode t)
(global-font-lock-mode t)
;;for large file show quickly
;(require 'lazy-lock)
;(setq lazy-lock-defer-on-scrolling t)
;(setq font-lock-support-mode 'lazy-lock-mode)

(setq font-lock-maximum-decoration t)
(setq font-lock-global-modes '(not text-mode))
(setq font-lock-verbose t)
(setq font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))

;;let F7, as in vim do, to insert the current
;;time-stamp, whose form is the same as vim do, into
;;current cursor point.
(defun insert-time-stamp ()
  "Insert date from the system time.
      Which is in \"\%Y-\%m-\%d \%H:\%M:\%S\" mode, as in vim do. "
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;;ftp client
(if (or (eq window-system 'w32)
        (eq window-system 'win32))
    (setq ange-ftp-ftp-program-name "ftp.exe"))

;;mouset avoidance
;(mouse-avoidance-mode 'animate)
(mouse-avoidance-mode 'exile)
(setq mouse-avoidance-threshold 10)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;Diary Setting
(setq diary-file "~/diary/rj")
(setq calendar-latitude +39.9)
(setq calendar-longitude +116.4)
(setq calendar-location-name "shenyang")
(setq calendar-remove-frame-by-deleting t)
(setq calendar-week-start-day 1)

(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq islamic-holidays nil)

(setq general-holidays
      '((holiday-fixed 1  1     "New Year's Day")
        (holiday-fixed 3  8     "Women's Day")
        (holiday-fixed 4  1     "April Fool's Day")
        (holiday-fixed 5  1     "Labor Day")
        (holiday-fixed 10 1     "National Day")
        (holiday-fixed 12 25    "Christmas")
        (holiday-fixed 2  5     "Lantern Festival")
        (holiday-fixed 4  4     "Ching Ming Festival")
        (holiday-fixed 6  22    "Dragon Boat Festival")
        (holiday-fixed 9  28    "Mid-Autumn Festival")
        (holiday-float 5  0  2  "Mother's Day")
        (holiday-float 6  0  3  "Father's Day")))

(setq mark-diary-entries-in-calendar nil)
(setq mark-holidays-in-calendar t)
(setq view-calendar-holidays-initially t)

;;settings for appt
(require 'appt)
(setq appt-issue-message nil)
(setq appt-message-warning-time 3)
(setq appt-display-format 'window)

;;chinese-calendar
(setq chinese-calendar-celestial-stem
       ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
       ["子" "丑" "寅" "卯" "辰" "巳" "戊" "未" "申" "酉" "戌" "亥"])
 
;;work direction
;(setq default-directory "~/work")

(setq column-number-mode t)
(setq default-fill-column 80)

;;'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

(setq x-stretch-cursor nil)

(setq-default kill-whole-line t)

;;//auto load: transient-mark-mode,delete-selection-mode
(if (fboundp 'pc-selection-mode)                                              
    (pc-selection-mode)                                                   
    (require 'pc-select))
(custom-set-variables '(pc-selection-mode t nil (pc-select)))

;;Non-nil if Transient-Mark mode is enabled.
(setq-default transient-mark-mode t)
(setq-default delete-selection-mode t)
(setq pc-select-selection-keys-only t)

;;scroll properity
(setq-default scroll-step              1
              scroll-conservatively    most-positive-fixnum
              scroll-up-aggressively   0.0
              scroll-down-aggressively 0.0)

(setq kill-ring-max 200)

;;disable auto wrap
(setq truncate-partial-width-windows nil)

(winner-mode t)

;;buffer name in title
(setq frame-title-format
      (list
       ;;(user-login-name)
       "emacs@"
       (replace-regexp-in-string "\\..*$" ""system-name)
       ":"
       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq gnus-inhibit-startup-message t)

(setq default-major-mode 'text-mode)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq truncate-partial-width-windows nil)

;;indent setting
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-stop-list ())
(dotimes (i 40 tab-stop-list)
  (setq tab-stop-list
        (cons (* (- 40 i) default-tab-width)
              tab-stop-list)))

(setq mouse-yank-at-point t)
(auto-compression-mode t)
(column-number-mode t)

;;display local-mode calendar
(setq display-time-string-forms
      '("["24-hours":"minutes","dayname","monthname" "day","year"]"))
(display-time)

;;embrace light show
(show-paren-mode t)

;(if (fboundp 'menu-bar-mode)    (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'tool-bar-mode)     (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)   (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode)      (tooltip-mode -1))

;;If tooltips turned on, make tips appear promptly
(setq tooltip-delay 0.1) ; default is one second

;;Minibuffer complete help
(icomplete-mode t)

;;mouse wheel support
(setq mouse-wheel-mode t)

;;ensure abbrev mode is always on
(setq-default abbrev-mode t)
;;do not bug me about saving my abbreviations
(setq save-abbrevs nil)

;;ido mode
(setq ido-save-directory-list-file nil)
(ido-mode t)

(custom-set-faces
 '(ido-subdir            ((t (:foreground "#66ff00"))))
 '(ido-first-match       ((t (:foreground "#ccff66"))))
 '(ido-only-match        ((t (:foreground "#ffcc33"))))
 '(ido-indicator         ((t (:foreground "#ffffff"))))
 '(ido-incomplete-regexp ((t (:foreground "#ffffff")))))

(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "M-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "M-p") 'ido-prev-match)))

;;only auto spit windows
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;;hide buffer *Async Shell Command*
(defadvice display-buffer (around async-shell-command activate)
   "If BUFFER is named *Async Shell Command*, don't display it."
   (or (and (bufferp (ad-get-arg 0))
            (equal (buffer-name (ad-get-arg 0)) "*Async Shell Command*"))
       ad-do-it))

;;recentf/undo-kill-buffer
(setq recentf-menu-open-all-flag t
      recentf-max-saved-items 100
      recentf-max-menu-items  30
      recentf-exclude '("/tmp/" "/ssh:"))

(recentf-mode t)
(defadvice recentf-track-closed-file (after push-beginning activate)
  "Move current buffer to the beginning of the recent list after killed."
  (recentf-track-opened-file))

(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed. With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf))))
                           (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delete buf-file recently-killed-list)))
     buffer-files-list)
    (find-file (nth (- arg 1) recently-killed-list))))

(defun recentf-open-files-compl ()
   (interactive)
   (let* ((all-files recentf-list)
     (tocpl (mapcar (function 
       (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
           (prompt (append '("File name: ") tocpl))
          (fname (completing-read (car prompt) (cdr prompt) nil nil)))
     (find-file (cdr (assoc-ignore-representation fname tocpl)))))

;;esc quits
(define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

;;manpath
(defun my-woman-at-point ()
  (interactive)
  (let ((woman-topic-at-point t))
    (woman)))

;;do not open new frame
(setq woman-use-own-frame nil)

(unless (or (eq window-system 'w32)
            (eq window-system 'win32))
  (setq woman-manpath (quote ("/usr/share/man"))))


(provide 'common-setting)

;;; common-setting.el ends here
