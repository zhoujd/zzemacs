;;; zz-theme.el --- TextMate Blackboard Theme

(deftheme zz
  "Based on Color theme by JD Huntington, which based off the TextMate Blackboard theme")

(custom-theme-set-faces
 `zz
 `(default ((t (:background "black" :foreground "#F8F8F8" ))))
 `(bold ((t (:bold t))))
 `(bold-italic ((t (:bold t))))
 `(border-glyph ((t (nil))))
 `(buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
 `(font-lock-builtin-face ((t (:foreground "#94bff3"))))
 `(font-lock-comment-face ((t (:italic t :foreground "#AEAEAE"))))
 `(font-lock-constant-face ((t (:foreground "#D8FA3C"))))
 `(font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
 `(font-lock-function-name-face ((t (:foreground "#FF6400"))))
 `(font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
 `(font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
 `(font-lock-reference-face ((t (:foreground "SlateBlue"))))

 `(font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
 `(font-lock-regexp-grouping-construct ((t (:foreground "red"))))

 ;; org-mode
 `(org-hide ((t (:foreground "#2e3436"))))
 `(org-level-1 ((t (:bold nil :foreground "dodger blue" :height 1.3))))
 `(org-level-2 ((t (:bold nil :foreground "#edd400" :height 1.2))))
 `(org-level-3 ((t (:bold nil :foreground "#6ac214" :height 1.1))))
 `(org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))
 `(org-date ((t (:underline t :foreground "magenta3"))))
 `(org-footnote  ((t (:underline t :foreground "magenta3"))))
 `(org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
 `(org-special-keyword ((t (:foreground "brown"))))
 `(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
 `(org-block ((t (:foreground "#bbbbbc"))))
 `(org-quote ((t (:inherit org-block :slant italic))))
 `(org-verse ((t (:inherit org-block :slant italic))))
 `(org-todo ((t (:bold t :foreground "Red"))))
 `(org-done ((t (:bold t :foreground "ForestGreen"))))
 `(org-agenda-structure ((t (:weight bold :foreground "tomato"))))
 `(org-agenda-date ((t (:foreground "#6ac214"))))
 `(org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
 `(org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))

 ;; helm
 `(helm-buffer-directory ((t (:foreground "#c350ff"))))
 `(helm-ff-directory ((t (:foreground "#c350ff"))))
 `(helm-ff-symlink ((t (:foreground "#f6df92"))))
 `(helm-ff-prefix ((t (:foreground "#ff5f87" :weight bold))))
 `(helm-source-header ((t (:foreground "#ff5f87" :underline t :weight bold))))

 `(helm-selection ((t (:background "#333" :underline t))))
 `(helm-selection-line ((t (:background "#ccc" :foreground "#ff5f87" :underline nil))))

 `(helm-candidate-number ((t (:foreground "#ff5f87" :bold t))))
 `(helm-match ((t (:foreground "#ff5f87"))))

 ;; misc
 `(font-lock-string-face ((t (:foreground "#61CE3C"))))
 `(font-lock-type-face ((t (:foreground "#8DA6CE"))))
 `(font-lock-variable-name-face ((t (:foreground "#FF6400"))))
 `(font-lock-warning-face ((t (:bold t :foreground "pink"))))
 `(gui-element ((t (:background "#D4D0C8" :foreground "black"))))
 `(region ((t (:background "#253B76"))))
 `(mode-line ((t (:background "grey75" :foreground "black"))))
 `(highlight ((t (:background "#222222"))))
 `(highline-face ((t (:background "SeaGreen"))))
 `(italic ((t (nil))))
 `(left-margin ((t (nil))))
 `(text-cursor ((t (:background "yellow" :foreground "black"))))
 `(toolbar ((t (nil))))
 `(underline ((nil (:underline nil))))
 `(zmacs-region ((t (:background "snow" :foreground "ble")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zz)

;;; zz-theme.el ends here.
