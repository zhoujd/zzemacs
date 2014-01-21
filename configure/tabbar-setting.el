;;;;tabbar-mode setting

;;tabbar mode
;(if window-system (require 'tabbar-ruler) (require 'tabbar))
;(require 'tabbar-rose)
(require 'tabbar)
;;turn on the tabbar
(tabbar-mode t)

;;don't show help information
(setq tabbar-scroll-left-help-function nil)   
(setq tabbar-scroll-right-help-function nil)
(setq tabbar-help-on-tab-function nil)
(setq tabbar-home-help-function nil)
;;don't show tabbar button
(setq tabbar-buffer-home-button (quote (("") "")))
(setq tabbar-scroll-left-button (quote (("") "")))
(setq tabbar-scroll-right-button (quote (("") "")))
(setq tabbar-mwheel-mode nil)

;;Excluded buffers in tabbar
(setq tabbar-excluded-buffers '("*Ido Completions*" "*Completions*"
                                "*ESS*" "*Pymacs*" "*WoMan-Log*"))

(defun tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or *, when they are not
visiting a file.  The current buffer is always included."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                      ;; Always include the current buffer.
                      ((eq (current-buffer) b) b)
                      ((buffer-file-name b) b)
                      ((member (buffer-name b) tabbar-excluded-buffers) nil)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      ((buffer-live-p b) b)))
                (buffer-list))))

(setq tabbar-buffer-list-function 'tabbar-buffer-list)

;;define all tabs to be one of 2 possible groups: "Emacs Buffer", "User Buffer".
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with *."
  (list
   (cond
    ((or (string-match "\\*.*\\*"  (buffer-name))
         (string-match "^ "  (buffer-name)))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
      "Dired"
     )
    (t
     "User Buffer"
     )
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;;tabbar font name setting
(if (boundp 'my-font-en-name)
    (setq tabbar-font-name (car (split-string my-font-en-name)))
    (setq tabbar-font-name "Consolas"))

(if window-system
    (setq tabbar-bgcolor "#AAAAAA")
    (setq tabbar-bgcolor "#75507B"))

(set-face-attribute 'tabbar-default nil
                    :inherit    nil
                    :weight    'normal
                    :width     'normal
                    :slant     'normal
                    :underline  nil
                    :strike-through nil
                    :stipple    nil
                    :background tabbar-bgcolor
                    :foreground "black"
                    :box    nil
                    :family tabbar-font-name)
(set-face-attribute 'tabbar-selected nil
                    :background "LightGoldenrod"
                    :foreground "DarkGreen"
                    :inherit    'tabbar-default 
                    :box (list :line-width 2 :color "LightGoldenrod" :style nil)
                    :weight 'bold)
(set-face-attribute 'tabbar-unselected nil
                    :inherit    'tabbar-default
                    :background tabbar-bgcolor
                    :box (list :line-width 2 :color tabbar-bgcolor :style nil))
(set-face-attribute 'tabbar-button nil
                    :inherit 'tabbar-default
                    :box nil)
(set-face-attribute 'tabbar-separator nil
                    :background "grey50"
                    :foreground "grey50"
                    :height 1)


(provide 'tabbar-setting)
