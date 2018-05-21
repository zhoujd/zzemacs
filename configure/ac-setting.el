;;;; ac-setting.el --- auto complete file

;; auto complete
(zz-load-path "site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat zzemacs-path "/site-lisp/auto-complete/dict"))
(ac-config-default)

(defun ac-next-or-next-line (arg)
  (interactive "p")
  (if (/= (length ac-candidates) 1)
      (ac-next)
      (ac-abort)
    (next-line arg)))
(defun ac-previous-or-previous-line (arg)
  (interactive "p")
  (if (/= (length ac-candidates) 1)
      (ac-previous)
      (ac-abort)
    (previous-line arg)))

(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline  'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

(dolist (command `(backward-delete-char-untabify delete-backward-char))
        (add-to-list 'ac-trigger-commands command))

(defun ac-start-use-sources (sources)
  (interactive)
  (let ((ac-sources sources))
    (call-interactively 'ac-start)))

(defvar ac-trigger-edit-commands
  `(self-insert-command
    delete-backward-char
    backward-delete-char
    backward-delete-char-untabify)
  "*Trigger edit commands that specify whether `auto-complete' should start or not when `ac-completing'.")

;; yasnippet show complete with  auto-complete
(defun ac-yasnippet-candidate ()
  (let ((table (yas/get-snippet-tables major-mode)))
    (if table
      (let (candidates (list))
            (mapcar (lambda (mode)
                      (maphash (lambda (key value)
                                 (push key candidates))
                               (yas/snippet-table-hash mode)))
                    table)
            (all-completions ac-prefix candidates)))))


(provide 'ac-setting)

;;;; ac-setting.el --- end here
