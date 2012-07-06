;;; Color theme based on Tango Palette.
;;; Created by zhuqin <zhuqin83@gmail.com>
;; color plate
;; Butter       fce94f      edd400      c4a000
;; Orange       fcaf3e      f57900      ce5c00
;; Chocolate    e9b96e      c17d11      8f5902
;; Chameleon    8ae234      73d216      4e9a06
;; Sky Blue     729fcf      3465a4      204a87
;; Plum         ad7fa8      75507b      5c3566
;; Scarlet Red  ef2929      cc0000      a40000
;; Aluminium    eeeeec      d3d7cf      babdb6
;;              888a85      555753      2e3436

(defun color-theme-tango-dark ()
  "A color theme based on Tango Palette."
  (interactive)
  (color-theme-install
   '(color-theme-tango-dark
     ((background-color . "#121212")
      (background-mode . dark)
      (border-color . "#888a85")
      (foreground-color . "#eeeeec")
      (cursor-color . "#888888")
      (mouse-color . "#333333"))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))
     (font-lock-builtin-face ((t (:foreground "#729fcf"))))
     (font-lock-comment-face ((t (:foreground "#888a85"))))
     (font-lock-constant-face ((t (:foreground "#ad7fa8"))))
     (font-lock-doc-face ((t (:foreground "#888a85"))))
     (font-lock-function-name-face ((t (:foreground "#729fcf"))))
     (font-lock-keyword-face ((t (:foreground "#fcaf3e"))))
     (font-lock-string-face ((t (:foreground "#73d216"))))
     (font-lock-type-face ((t (:foreground "#c17d11"))))
     (font-lock-variable-name-face ((t (:foreground "#fce94f"))))
     (font-lock-warning-face ((t (:bold t :foreground "#cc0000"))))

     (border ((t (:background "#888a85"))))
     ;;(fringe ((t (:background "#111111"))))
     (highlight ((t (:background "#444444"))))

     ;(mode-line ((t (:foreground "#eeeeec" :background "#2e3436" :box (:line-width 1 :style released-button)))))
     ;(mode-line-inactive ((t (:foreground "#cccddd" :background "#111111"))))

     (modeline ((t (:background "gray" :foreground "black" :box (:line-width 1 :style released-button)))))
     (modeline-buffer-id ((t (:background "gray" :foreground "black"))))
     (modeline-mousable ((t (:background "gray" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "gray" :foreground "black"))))

     (region ((t (:background "#555753"))))

     (flyspell-duplicate ((t (:foreground "#fcaf3e"))))
     (flyspell-incorrect ((t (:foreground "#cc0000"))))

     (org-date ((t (:foreground "LightSteelBlue" :underline t))))
     (org-hide ((t (:foreground "#2e3436"))))
     (org-todo ((t (:inherit font-lock-keyword-face :bold t))))
     (org-level-1 ((t (:inherit font-lock-function-name-face))))
     (org-level-2 ((t (:inherit font-lock-variable-name-face))))
     (org-level-3 ((t (:inherit font-lock-keyword-face))))
     (org-level-4 ((t (:inherit font-lock-string-face))))
     (org-level-5 ((t (:inherit font-lock-constant-face))))

     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground "#8ae234"))))
     (isearch ((t (:background "#f57900" :foreground "#2e3436"))))
     (isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
     (paren-face-match ((t (:inherit show-paren-match-face))))
     (paren-face-match-light ((t (:inherit show-paren-match-face))))
     (paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
     (persp-selected-face ((t (:foreground "#729fcf"))))
     (show-paren-match-face ((t (:background "#729fcf" :foreground "#eeeeec"))))
     (show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))
     (minibuffer-prompt ((t (:foreground "#729fcf"))))
     (info-xref ((t (:foreground "#729fcf"))))
     (info-xref-visited ((t (:foreground "#ad7fa8"))))
     )))

(provide 'color-theme-tango)

