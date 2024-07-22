;;;; ui-setting.el --- ui config file

;;https://github.com/gexplorer/simple-modeline
(zz:load-path "site-lisp/simple-modeline")
(require 'simple-modeline)
(simple-modeline-mode t)

(defun zz:segment-percent-prefix ()
  "Return the percent prefix."
  (concat " "))

(defun zz:segment-percent-suffix ()
  "Return the percent suffix."
  (concat "%%" " "))

(defun zz:segment-percent-position ()
  "Return the percent position of the cursor in the current buffer."
  (setq mode-line-percent-position '(-3 "%p")))

(defun zz:segment-position ()
 "Displays the current cursor position in the mode-line."
 `((line-number-mode
    ((column-number-mode
      (column-number-indicator-zero-based
       (8 " %l:%c")
       (8 " %l:%C"))
      (5 " L%l")))
    ((column-number-mode
      (column-number-indicator-zero-based
       (5 " C%c")
       (5 " C%C")))))))

(defun zz:segment-region ()
   (if (region-active-p)
        (propertize (format " +%s"
                            (apply #'+ (mapcar
                                       (lambda (pos)
                                         (- (cdr pos)
                                            (car pos)))
                                       (region-bounds))))
                    'font-lock-face 'font-lock-variable-name-face)))

(defun zz:segment-major-mode ()
  "Displays the current major mode in the mode-line."
  `(" "
    (:propertize ("" mode-name)
               help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
               mouse-face mode-line-highlight
               local-map ,mode-line-major-mode-keymap)))

(defun zz:segment-end ()
  "Return the end of mode line."
  (concat " "))

(defun zz:replace-git-status (tstr)
  (let* ((tstr (replace-regexp-in-string "Git" "" tstr))
         (first-char (substring tstr 0 1))
         (rest-chars (substring tstr 1)))
    (cond
     ((string= ":" first-char) ;;; Modified
      (replace-regexp-in-string "^:" "⚡️" tstr))
     ((string= "-" first-char) ;; No change
      (replace-regexp-in-string "^-" "✔️" tstr))
     (t tstr))))
(advice-add #'vc-git-mode-line-string :filter-return
            #'zz:replace-git-status)

(setq simple-modeline-segments
      '((simple-modeline-segment-modified
         simple-modeline-segment-buffer-name
         zz:segment-percent-prefix
         zz:segment-percent-position
         zz:segment-percent-suffix
         zz:segment-position
         zz:segment-region)
        (simple-modeline-segment-input-method
         simple-modeline-segment-eol
         simple-modeline-segment-encoding
         simple-modeline-segment-vc
         simple-modeline-segment-misc-info
         simple-modeline-segment-process
         zz:segment-major-mode
         zz:segment-end)))

;;modern-fringes
(require 'modern-fringes)
(modern-fringes-invert-arrows)


(provide 'ui-setting)

;;;; ui-setting.el --- end here
