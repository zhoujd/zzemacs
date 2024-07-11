;;;; ui-setting.el --- ui config file

;;https://github.com/gexplorer/simple-modeline
(zz:load-path "site-lisp/simple-modeline")
(require 'simple-modeline)
(simple-modeline-mode t)

(defun simple-modeline-segment-percent-position ()
  "Return the percent position of the cursor in the current buffer."
  (format-mode-line (concat " " "%p")))

(defun simple-modeline-segment-end ()
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
         simple-modeline-segment-percent-position
         simple-modeline-segment-position)
        (simple-modeline-segment-input-method
         simple-modeline-segment-eol
         simple-modeline-segment-encoding
         simple-modeline-segment-vc
         simple-modeline-segment-misc-info
         simple-modeline-segment-process
         simple-modeline-segment-major-mode
         simple-modeline-segment-end)))

;;modern-fringes
(require 'modern-fringes)
(modern-fringes-invert-arrows)


(provide 'ui-setting)

;;;; ui-setting.el --- end here
