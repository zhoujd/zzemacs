;;;; common-setting.el --- common config file
;;

;;tell me if there's something wrong
;;(setq debug-on-error t)       ; goes into debug mode on errors
;;(setq debug-on-signal t)      ; goes into debug mode on signal
;;(setq debug-on-quit t)        ; goes into debug mode when C-g is entered
;;(setq inhibit-default-init t) ; disable loading of "default.el" at startup

;;enable debug
;;M-x toggle-debug-on-error

(zz/load-path "site-lisp")

(keyboard-translate ?\C-h ?\C-?)  ; translate 'C-h' to Backspace

;;transparency 'alpha '(<active> . <inactive>) or 'alpha <both>
;;https://www.emacswiki.org/emacs/TransparentEmacs
(set-frame-parameter (selected-frame) 'alpha 90)
(add-to-list 'default-frame-alist '(alpha 90))

;;Chinese
(defun zz/set-language-chinese ()
  "This is for chinese setting"
  (interactive)
  (set-language-environment 'Chinese-GB18030)
  (set-buffer-file-coding-system 'chinese-gb18030)
  (message "This is for chinese"))

;;Japanese
(defun zz/set-language-japanese ()
  "This is for japanese setting"
  (interactive)
  (set-language-environment 'Japanese)
  (set-buffer-file-coding-system 'japanese-shift-jis)
  (message "This is for japanese"))

;;utf-8
(defun zz/set-language-utf-8 ()
  "This is for utf-8 setting"
  (interactive)
  (set-language-environment 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (message "This is for utf-8"))

;;Language switch
(cond
  ;;((string-match "j[ap].*" (getenv "LANG")) (zz/set-language-japanese))
  ;;((string-match "\\(zh_CN\\)\\|\\(CHS\\)" (getenv "LANG")) (zz/set-language-chinese))
  (t (zz/set-language-utf-8)))

;;font setting
;;tools: xlsfonts or xfontsel
;;JetBrains Mono: https://www.jetbrains.com/lp/mono/
;;Spleen: https://github.com/fcambus/spleen
;;https://github.com/supercomputra/SF-Mono-Font
;;https://github.com/soytony/SF-Mono-SC
;;https://github.com/ryanoasis/nerd-fonts/
;;https://www.programmingfonts.org/
;;M-x menu-set-font
;;(set-face-font 'default "-*-WenQuanYi Zen Hei Mono-*-*-*-*-15-*-*-*-*-*-*-*")
;;(custom-set-faces '(default ((t (:family "WenQuanYi Zen Hei Mono" :size 15)))))
(defconst zz/en-font-list '(
                            "SF Mono 13"
                            "SF Mono Nerd 13"
                            "JetBrains Mono NL 13"
                            "Droid Sans Mono SW 13"
                            "WenQuanYi Zen Hei Mono 13"
                            ))
(defconst zz/cn-font-list '(
                            "SF Mono SC"
                            "Droid Sans Fallback"
                            "Microsoft YaHei Mono"
                            ))
(defconst zz/console-font-list '(
                                 "-misc-spleen-medium-r-normal--64-640-72-72-c-320-iso10646-1"
                                 "-*-WenQuanYi Zen Hei Mono-*-*-*-*-15-*-*-*-*-*-*-*"
                                 ))

(defun zz/frame-font (font-en-name &optional font-cn-name)
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
    (zz/frame-font (nth 0 zz/en-font-list))
    (if (display-graphic-p)
        (zz/frame-font (nth 0 zz/en-font-list) (nth 0 zz/cn-font-list))
        (set-face-font 'default (nth 0 zz/console-font-list))))

;;improve theme loading
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

;;color theme
(zz/load-path "site-lisp/emacs-gruvbox-themes")
(require 'gruvbox-theme)
(load-theme 'gruvbox t)

;;default-frame-alist or initial-frame-alist
(add-to-list 'default-frame-alist '(mouse-color . "white"))

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

;;frame title
(setq frame-title-format
      '(:eval
        (format "emacs:%s %s %s@%s"
                (buffer-name)
                (file-name-nondirectory (abbreviate-file-name
                                         (directory-file-name default-directory)))
                (emacs-pid)
                (or (file-remote-p default-directory 'host) (system-name))
                )))

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

;;frame attribute
(defun new-frame-setup (frame)
  (if (display-graphic-p frame)
      (progn
        (custom-set-faces
         '(mode-line-inactive ((t :background "#32302f" :foreground "#000000" :box (:line-width 1 :color "#32302f"))))
         '(mode-line ((t :background "#504945" :foreground "#000000" :box (:line-width 1 :color "#504945")))))
        (message "Start window system"))
      (message "Start non-window system")))
;;run for already-existing frames
(mapc 'new-frame-setup (frame-list))
;;run when a new frame is created
(add-hook 'after-make-frame-functions 'new-frame-setup)

;;mode-line time
(setq display-time-string-forms
      '((propertize (format-time-string "WW%-V %a %e %b %l:%M %p"))))
(display-time-mode t)

(setq global-mode-string (remove 'display-time-string global-mode-string))
(setq mode-line-end-spaces
      (list (propertize " " 'display
                        `((space :align-to (- right
                                              ,(length display-time-string)))))
            'display-time-string))

;;mode-line format
(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                (:propertize ("" mode-line-mule-info mode-line-client
                              mode-line-modified mode-line-remote)
                             display (min-width (5.0)))
                mode-line-frame-identification mode-line-buffer-identification "   "
                mode-line-position (vc-mode vc-mode) "  "
                (:propertize ("" mode-name)
                             help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                             mouse-face mode-line-highlight
                             local-map ,mode-line-major-mode-keymap)
                mode-line-misc-info
                mode-line-end-spaces))

;;git status in mode-line
(defun zz/vc-git-mode-line-string (orig-fn &rest args)
  "Replace Git in modeline with font-awesome git icon via ORIG-FN and ARGS."
  (let ((str (apply orig-fn args)))
    (concat "" [#xF1D3] "")))
(advice-add #'vc-git-mode-line-string :around #'zz/vc-git-mode-line-string)

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

;;ido faces
;(custom-set-faces
; '(ido-subdir            ((t (:foreground "#66ff00"))))
; '(ido-first-match       ((t (:foreground "#ccff66"))))
; '(ido-only-match        ((t (:foreground "#ffcc33"))))
; '(ido-indicator         ((t (:foreground "#ffffff"))))
; '(ido-incomplete-regexp ((t (:foreground "#ffffff")))))

(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (defkeys-map ido-completion-map
              ((kbd "M-n") 'ido-next-match)
              ((kbd "M-p") 'ido-prev-match))))

;;only auto spit windows
(setq split-height-threshold 0
      split-width-threshold  9999)

;;hide buffer *Async Shell Command*
(defadvice display-buffer (around async-shell-command activate)
   "If BUFFER is named *Async Shell Command*, don't display it."
   (or (and (bufferp (ad-get-arg 0))
            (equal (buffer-name (ad-get-arg 0)) "\\*Async Shell Command\\*.*"))
       ad-do-it))

;;minibuffer esc quits
(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map))
  (define-key map [escape] 'minibuffer-keyboard-quit))

;;manpath
(defun zz/woman-at-point ()
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
(defvar zz/system-trash-flag t
  "Use system trash flag")
(unless zz/system-trash-flag
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
;;disable custom-file by mapping it to a temporary file
(setq custom-file (make-temp-name "/tmp/"))

;;remove cl warning on emacs27
(unless (< emacs-major-version 27)
  (defun zz/check-cl-warning()
    "debug `Package cl is"
    (interactive)
    (require 'loadhist)
    (dolist (file (file-dependents (feature-file 'cl)))
      (message "%s" file)))
  (setq byte-compile-warnings '(cl-functions)))

(when (version< emacs-version "28")
  (defvar completions-detailed nil))

;;disable lock files
(setq create-lockfiles nil)

;;kill process buffer without confirmation
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


(provide 'common-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; common-setting.el ends here
