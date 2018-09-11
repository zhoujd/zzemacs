;;;; common-setting.el --- common config file
;;

;;tell me if there's something wrong
;(setq debug-on-error t) ;; goes into debug mode on errors
;(setq debug-on-quit t)  ;; goes into debug mode when C-g is entered

(zz-load-path "site-lisp")

;;disable loading of "default.el" at startup
;(setq inhibit-default-init t)
;(keyboard-translate ?\C-h ?\C-?)  ; translate `C-h' to DEL
;(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'.

;;the current frame to make it transparent
(set-frame-parameter (selected-frame) 'alpha '(95 70))
(add-to-list 'default-frame-alist '(alpha 95 70))

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
  ;;((string-match "j[ap].*" (getenv "LANG")) (my-set-language-japanese))
  ;;((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG")) (my-set-language-chinese))
  (t (my-set-language-utf-8)))

;;font setting
;;tools: xlsfonts or xfontsel
;;M-x menu-set-font
;;(set-face-font 'default "-*-Microsoft YaHei Mono-*-*-*-*-17-*-*-*-*-*-*-*")
;;(custom-set-faces '(default ((t (:family "Consolas" :size 15)))))
(setq en-font-list '(
                     "Consolas 14"
                     "Consolas 16"
                     "Consolas 24"
                     "Anonymous Pro 14"
                     "Anonymous Pro 16"
                     "Anonymous Pro 24"
                     ))
(setq cn-font-list '(
                     "Microsoft YaHei Mono 14"
                     "Microsoft YaHei Mono 16"
                     "Microsoft YaHei Mono 24"
                     ))

(defun my-font-name (name)
  (when (string-match ".*[ ]" name)
    (setq my-cn-name (substring (match-string 0 name) 0 -1))))

(defun my-font-size (name)
  (string-to-number (car (last (split-string name)))))

(defun my-frame-font (font-en-name font-cn-name)
  "my frame font setting"
  ;; Setting English Font
  (set-face-attribute 'default nil :font font-en-name)
  ;; Setting Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family (my-font-name font-cn-name)
                                       :size   (my-font-size font-cn-name)))))

(defun my-console-font (font-console-name)
  "my console font setting"
  (when (member font-console-name (font-family-list))
    (add-to-list 'default-frame-alist
                 '(font . font-console-name))))

(defun primary-x-font ()
  (interactive)
  (if-ms-windows
   (my-frame-font (nth 0 en-font-list) (nth 0 cn-font-list))
   (my-frame-font (nth 0 en-font-list) (nth 0 cn-font-list))))

(defun secondary-x-font ()
  (interactive)
  (if-ms-windows
   (my-frame-font (nth 1 en-font-list) (nth 1 cn-font-list))
   (my-frame-font (nth 2 en-font-list) (nth 2 cn-font-list))))

;;console font setting
(if window-system
    (primary-x-font)
    (my-console-font (nth 0 cn-font-list)))

(defun my-use-server-mode ()
  ;;server-mode
  ;;emacsclientw.exe -f "~\.emacs.d\server\server" -n -a "runemacs.exe" path\to\file
  ;;emacsclientw.exe --server-file ~\.emacs.d\server\server -n -a runemacs.exe path\to\file
  (defvar server-directory-name "~/.emacs.d/server")
  (when-ms-windows
   (when (not (file-directory-p server-directory-name))
     (make-directory server-directory-name t)))

  (require 'server)
  (when-ms-windows
   ;;suppress error "directory
   ;;~/.emacs.d/server is unsafe on windows.
   (defun server-ensure-safe-dir (dir) "Noop" t))
  (unless (server-running-p)
    (server-start))

  (add-hook 'kill-emacs-hook
            (lambda ()
              (if (file-exists-p  (concat server-directory-name "/server"))
                  (delete-file (concat server-directory-name "/server")))))

  (message "start emacs server..."))

(when-ms-windows
 (defun my-use-gnusvr ()
   ;; start gnuserv on Windows
   (progn
     (require 'gnuserv)
     (setq server-done-function 'bury-buffer gnuserv-frame (car (frame-list)))
     (gnuserv-start)
      ;;; open buffer in existing frame instead of creating new one...
     (setq gnuserv-frame (selected-frame))
     (message "gnuserv started."))))

(if-ms-windows
 (let ((use-gnusvr-flag nil))
   (if use-gnusvr-flag
       (my-use-gnusvr)
       (my-use-server-mode)))
 (my-use-server-mode))

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

;;default-frame-alist or initial-frame-alist
(setq default-frame-alist (append '((mouse-color . "white")
                                    (scroll-bar-width . 16)
                                    (width .  110)
                                    (height . 32))
                                  default-frame-alist))

;;quick display key help
(setq echo-keystrokes 0.1)

;;scroll bar right
;(set-scroll-bar-mode `right)

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
(setq visible-bell t)
;;don`t flash the screen on console mode
;(setq ring-bell-function 'ignore)
(setq ring-bell-function (lambda () t))
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
(setq font-lock-global-modes t)
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

;;mouset avoidance
;(mouse-avoidance-mode 'animate)
(mouse-avoidance-mode 'exile)
(setq mouse-avoidance-threshold 10)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;Diary Setting
(setq diary-file "~/.diary")
(setq calendar-latitude +31.23)
(setq calendar-longitude +121.47)
(setq calendar-location-name "Shanghai")
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

(setq column-number-mode t)
(setq default-fill-column 80)

;;'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

(setq x-stretch-cursor nil)

(setq-default kill-whole-line t)

(setq transient-mark-mode t
      mark-even-if-inactive t
      set-mark-command-repeat-pop t)
(transient-mark-mode t)
(delete-selection-mode t)

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
(defun fname-title-string ()
  "Return the file name of current buffer, using ~ if under home directory"
  (let ((fname (or (buffer-file-name (current-buffer))
                   (buffer-name))))
    (when (string-match (getenv "HOME") fname)
      (setq fname (replace-match "~" t t fname))        )
    fname))

(setq frame-title-format
      '(:eval (concat (user-login-name) "@" (system-name) "[Emacs"
                      (nth 2 (split-string (version))) "]  " (fname-title-string))))

;suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq gnus-inhibit-startup-message t)

;;disable loading of "default.el" at startup
(setq inhibit-default-init t)

(setq default-major-mode 'text-mode)
(setq truncate-partial-width-windows nil)

;;indent setting
(setq-default tab-always-indent 'complete)
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

(unless (version< emacs-version "23.2")
  (setq global-mode-string (remove 'display-time-string global-mode-string))
  (setq mode-line-end-spaces
        (list (propertize " " 'display '(space :align-to (- right 23)))
              'display-time-string)))

;;embrace light show
(show-paren-mode t)

(if (fboundp 'menu-bar-mode)     (menu-bar-mode -1))
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
(require 'ido-complete-space-or-hyphen)
(setq ido-save-directory-list-file nil)
(setq ido-use-virtual-buffers nil)
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
(setq recentf-menu-open-all-flag  t
      recentf-max-saved-items     100
      recentf-max-menu-items      30)

(unless-ms-windows
  (setq recentf-exclude '("/tmp/" "/ssh:")))

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
          (tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
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

(unless-ms-windows
  (setq woman-manpath (quote ("/usr/share/man"))))

;(setq resize-mini-windows nil)
;(setq default-line-spacing 0)

;;set the mouse scroll wheel to move 1 line per event
;(setq mouse-wheel-scroll-amount '(1))

;;when deleted a file goes to the OS's trash folder:
(setq trash-directory "~/.Trash")
(setq delete-by-moving-to-trash t)

;;stop asking “Active processes exist; kill them and exit anyway”
;(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;  (flet ((process-list ())) ad-do-it))

;;M-x set-variable RET -> input: case-fold-search/case-replace RET->nil RET
(setq-default case-fold-search nil)   ; require exact matches
(setq-default case-replace nil)       ; never change case when replacing

;;configure saved from menu "Save Options"
(setq custom-file (concat zzemacs-path "/custom.el"))


(provide 'common-setting)

;;; common-setting.el ends here
