;;;; ui-setting.el --- ui config file

;;https://github.com/gexplorer/simple-modeline
(zz:load-path "site-lisp/simple-modeline")
(require 'simple-modeline)
(simple-modeline-mode)

(defun simple-modeline-segment-percent-position ()
  "Return the percent position of the cursor in the current buffer."
  (format-mode-line "%p"))

(setq simple-modeline-segments
      '((simple-modeline-segment-modified
         simple-modeline-segment-buffer-name
         simple-modeline-segment-position
         simple-modeline-segment-percent-position)
        (simple-modeline-segment-input-method
         simple-modeline-segment-eol
         simple-modeline-segment-encoding
         simple-modeline-segment-vc
         simple-modeline-segment-misc-info
         simple-modeline-segment-process
         simple-modeline-segment-major-mode)))

;;mode-line
;(zz:load-path "site-lisp/mood-line")
;(require 'mood-line)
;(setq mood-line-glyph-alist mood-line-glyphs-fira-code)
;(mood-line-mode t)

;;modern-fringes
(require 'modern-fringes)
(modern-fringes-invert-arrows)


(provide 'ui-setting)

;;;; ui-setting.el --- end here
