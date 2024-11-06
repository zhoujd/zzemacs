;;;; ui-setting.el --- ui config file

;;https://github.com/gexplorer/simple-modeline
(zz/load-path "site-lisp/simple-modeline")
(require 'simple-modeline)
(simple-modeline-mode t)

(defun zz/segment-percent-prefix ()
  "Return the percent prefix."
  (concat " "))

(defun zz/segment-percent-suffix ()
  "Return the percent suffix."
  (concat "%%" " "))

(defun zz/segment-percent-position ()
  "Return the percent position of the cursor in the current buffer."
  (setq mode-line-percent-position '(-3 "%p")))

(defun zz/segment-position ()
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

(defun zz/segment-region ()
   (if (region-active-p)
        (propertize (format " +%s"
                            (apply #'+ (mapcar
                                       (lambda (pos)
                                         (- (cdr pos)
                                            (car pos)))
                                       (region-bounds))))
                    'font-lock-face 'font-lock-variable-name-face)))

(defun zz/segment-major-mode ()
  "Displays the current major mode in the mode-line."
  `(" "
    (:propertize ("" mode-name)
               help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
               mouse-face mode-line-highlight
               local-map ,mode-line-major-mode-keymap)))

(defun zz/segment-end ()
  "Return the end of mode line."
  (concat ""))

(defun zz/segment-process ()
  "Return the current value of `mode-line-process'."
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string-blank-p process-info)
      (concat " " (string-trim process-info)))))

(defun zz/segment-anzu ()
  "Return color-coded anzu status information."
  (when (bound-and-true-p anzu--state)
    anzu--mode-line-format))

(defun zz/segment-remote ()
  "Return remote status information."
  (when (file-remote-p default-directory)
    (concat " " "@")))

(setq simple-modeline-segments
      '((zz/segment-anzu
         simple-modeline-segment-modified
         zz/segment-remote
         simple-modeline-segment-buffer-name
         zz/segment-percent-prefix
         zz/segment-percent-position
         zz/segment-percent-suffix
         zz/segment-position
         zz/segment-region)
        (simple-modeline-segment-input-method
         simple-modeline-segment-eol
         simple-modeline-segment-encoding
         simple-modeline-segment-misc-info
         zz/segment-process
         zz/segment-major-mode
         zz/segment-end)))

;;modern-fringes
(require 'modern-fringes)
(modern-fringes-invert-arrows)

;;awesome-tray
(zz/load-path "site-lisp/awesome-tray")
(require 'awesome-tray)
(setq awesome-tray-position 'right)
(setq awesome-tray-active-modules
      '(
        "buffer-read-only"
        "anzu"
        "location"
        "hostname"
        "buffer-name"
        "mode-name"
        ))


(provide 'ui-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; ui-setting.el --- end here
