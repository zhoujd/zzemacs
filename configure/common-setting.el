;;;; common-setting.el --- common config file
;;

;;tell me if there's something wrong
;;(setq debug-on-error t)       ; goes into debug mode on errors
;;(setq debug-on-quit t)        ; goes into debug mode when C-g is entered
;;(setq inhibit-default-init t) ; disable loading of "default.el" at startup

;;enable debug
;;M-x toggle-debug-on-error

(zz:load-path "site-lisp")

(keyboard-translate ?\C-h ?\C-?)  ; translate 'C-h' to Backspace

;;transparency 'alpha '(<active> . <inactive>) or 'alpha <both>
;;https://www.emacswiki.org/emacs/TransparentEmacs
(set-frame-parameter (selected-frame) 'alpha 90)
(add-to-list 'default-frame-alist '(alpha 90))

;;Chinese
(defun zz:set-language-chinese ()
  "This is for chinese setting"
  (interactive)
  (set-language-environment 'Chinese-GB18030)
  (set-buffer-file-coding-system 'chinese-gb18030)
  (message "This is for chinese"))

;;Japanese
(defun zz:set-language-japanese ()
  "This is for japanese setting"
  (interactive)
  (set-language-environment 'Japanese)
  (set-buffer-file-coding-system 'japanese-shift-jis)
  (message "This is for japanese"))

;;utf-8
(defun zz:set-language-utf-8 ()
  "This is for utf-8 setting"
  (interactive)
  (set-language-environment 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (message "This is for utf-8"))

;;Language switch
(cond
  ;;((string-match "j[ap].*" (getenv "LANG")) (zz:set-language-japanese))
  ;;((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG")) (zz:set-language-chinese))
  (t (zz:set-language-utf-8)))

;;font setting
;;tools: xlsfonts or xfontsel
;;more fonts: https://github.com/ryanoasis/nerd-fonts
;;JetBrains Mono: https://www.jetbrains.com/lp/mono/
;;M-x menu-set-font
;;(set-face-font 'default "-*-WenQuanYi Zen Hei Mono-*-*-*-*-15-*-*-*-*-*-*-*")
;;(custom-set-faces '(default ((t (:family "WenQuanYi Zen Hei Mono" :size 15)))))
(defconst zz:en-font-list '(
                            "SF Mono 13"
                            "Consolas 13"
                            "JetBrains Mono NL 11"
                            "WenQuanYi Zen Hei Mono 13"
                            "Droid Sans Mono Slashed 11"
                            ))
(defconst zz:cn-font-list '(
                            "Droid Sans Fallback"
                            "Microsoft YaHei Mono"
                            ))
(defconst zz:console-font-list '(
                                 "WenQuanYi Zen Hei Mono 13"
                                 ))

(defun zz:frame-font (font-en-name &optional font-cn-name)
  "frame font setting"
  ;; Setting English Font
  (set-face-attribute 'default nil :font font-en-name)
  (add-to-list 'default-frame-alist (cons 'font font-en-name))
  ;; Setting Chinese Font
  (when font-cn-name
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family font-cn-name)))))

;;console font setting
;;emacs daemon goes console font
(if (daemonp)
    (zz:frame-font (nth 0 zz:en-font-list))
    (if (display-graphic-p)
        (zz:frame-font (nth 0 zz:en-font-list) (nth 0 zz:cn-font-list))
        (zz:frame-font (nth 0 zz:console-font-list))))

;;improve theme loading
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

;;color theme
(zz:load-path "site-lisp/emacs-color-themes")
(require 'emacs-color-themes)
(if (display-graphic-p)
    (progn
      (load-theme 'zz t))
    (progn
      ;;color for console
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
(defun zz:insert-time-stamp ()
  "Insert date from the system time.
      Which is in \"\%Y-\%m-\%d \%H:\%M:\%S\" mode, as in vim do. "
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;;mouse avoidance
;;opt:animate,exile
(mouse-avoidance-mode 'exile)
(setq mouse-avoidance-threshold 10)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;Diary Setting
(setq diary-file "~/.diary")
(setq calendar-latitude +34.41)
(setq calendar-longitude +121.29)
(setq calendar-location-name "Shanghai")
(setq calendar-remove-frame-by-deleting t)

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

;;chinese-calendar
(setq chinese-calendar-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "戊" "未" "申" "酉" "戌" "亥"])

;;settings for appt
(require 'appt)
(setq appt-issue-message nil)
(setq appt-message-warning-time 3)
(setq appt-display-format 'window)

(setq column-number-mode t)
(setq default-fill-column 80)

;;auto fill mode
;;(add-hook 'text-mode-hook 'auto-fill-mode)

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
(defun zz:fname-title-string ()
  "Return the file name of current buffer, using ~ if under home directory"
  (let ((fname (or (buffer-file-name (current-buffer))
                   (buffer-name))))
      (replace-regexp-in-string (getenv "HOME") "~" fname)))

(setq frame-title-format
      '(:eval
        (format "%s@%s [Emacs%s] %s"
                (user-login-name)
                (system-name)
                (nth 2 (split-string (version)))
                (zz:fname-title-string))))

;;suppress GUI features
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

;;mode-line face attribute
(custom-set-faces
 '(mode-line-inactive ((t :background "#353644" :foreground "black" :box (:line-width 2 :color "#353644"))))
 '(mode-line ((t :background "#565063" :foreground "black" :box (:line-width 2 :color "#565063")))))

(setq display-time-string-forms
      '((propertize (format-time-string "%a %b %d %l:%M %p"))))
(display-time-mode t)

(setq global-mode-string (remove 'display-time-string global-mode-string))
(setq mode-line-end-spaces
      (list (propertize " " 'display
                        `((space :align-to (- (- right right-fringe)
                                              ,(string-width display-time-string)))))
            'display-time-string))

;;M-x display-time-world
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
(setq display-time-world-time-format "%10a %5b %2e %10R %5Z"
      display-time-world-list '(("Asia/Shanghai"       "Shanghai")
                                ("Asia/Calcutta"       "Bangalore")
                                ("Europe/London"       "London")
                                ("America/Los_Angeles" "Los Angeles")
                                ("America/New_York"    "New York")))

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

;;ensure abbrev mode is always on
(setq-default abbrev-mode t)
;;do not bug me about saving my abbreviations
(setq save-abbrevs nil)

;;ido mode
;;https://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido-complete-space-or-hyphen)
(setq ido-save-directory-list-file nil)
(setq ido-use-virtual-buffers nil)
(setq ido-enable-flex-matching nil)
(require 'ido)
(ido-mode t)

(custom-set-faces
 '(ido-subdir            ((t (:foreground "#66ff00"))))
 '(ido-first-match       ((t (:foreground "#ccff66"))))
 '(ido-only-match        ((t (:foreground "#ffcc33"))))
 '(ido-indicator         ((t (:foreground "#ffffff"))))
 '(ido-incomplete-regexp ((t (:foreground "#ffffff")))))

(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (defkeys-map ido-completion-map
              ((kbd "M-n") 'ido-next-match)
              ((kbd "M-p") 'ido-prev-match))))

;;only auto spit windows
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;;hide buffer *Async Shell Command*
(defadvice display-buffer (around async-shell-command activate)
   "If BUFFER is named *Async Shell Command*, don't display it."
   (or (and (bufferp (ad-get-arg 0))
            (equal (buffer-name (ad-get-arg 0)) "\\*Async Shell Command\\*.*"))
       ad-do-it))

;;recentf
(require 'recentf)
(require 'recentf-ext)
(recentf-mode t)
(setq recentf-menu-open-all-flag  t
      recentf-max-saved-items     30
      recentf-max-menu-items      30)
;;ignore some files
(setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                        "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                        ".*png$" ".*cache$"))

(defadvice recentf-track-closed-file (after push-beginning activate)
  "Move current buffer to the beginning of the recent list after killed."
  (recentf-track-opened-file))

(defun zz:undo-kill-buffer (arg)
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

(defun zz:recentf-open-files-compl ()
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
(defun zz:woman-at-point ()
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
;;Ubuntu system: .local/share/Trash/files
(defvar zz:system-trash-flag t
  "Use system trash flag")
(unless zz:system-trash-flag
  (setq trash-directory "~/.trash"))
(setq delete-by-moving-to-trash t)

;;stop asking “Active processes exist; kill them and exit anyway”
;(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;  (flet ((process-list ())) ad-do-it))

;;displays CFS when the searching is case-insensitive, otherwise it shows nothing.
(add-to-list 'minor-mode-alist '(case-fold-search " CFS"))
(defalias 'c 'toggle-case-fold-search)

;;configure saved from menu "Save Options"
;;normal location: ~/.emacs.d/custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))

;;auto refresh remote files
(custom-set-variables
 '(auto-revert-remote-files t))

;;remove cl warning on emacs27
(unless (< emacs-major-version 27)
  (defun zz:check-cl-warning()
    "debug `Package cl is"
    (interactive)
    (require 'loadhist)
    (dolist (file (file-dependents (feature-file 'cl)))
      (message "%s" file)))
  (setq byte-compile-warnings '(cl-functions)))

;;xterm use mouse
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(when (version< emacs-version "28")
  (defvar completions-detailed nil))


(provide 'common-setting)

;;; common-setting.el ends here
