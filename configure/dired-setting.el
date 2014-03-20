;;;; dired-setting.el --- dired config file
;;;

;;for dired-x tools
(require 'dired-x)

;;allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ;;"always" means no asking
(setq dired-recursive-deletes (quote top)) ;;"top" means ask once

;;Now, go to dired, then call split-window-vertically,
;;then go to another dired dir. Now, when you press C to copy,
;;the other dir in the split pane will be default destination.
;;Same for R (rename; move).
(setq dired-dwim-target t)

;;The same buffer for viewing directory, instead of spawning many
;;In dired, you can press 'a' instead of 'Enter' to open the dir
;;Disabled commands
(put 'dired-find-alternate-file 'disabled nil)

;;If you want Enter ↵ and ^ (parent dir) to use the same buffer
;;put the following in your emacs init file:
;(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
;(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

;;http://www.emacswiki.org/emacs/w32-browser.el
(when-ms-windows  
  (require 'w32-browser)
  (eval-after-load "dired"
    '(define-key dired-mode-map [C-f4] (lambda ()
                                         (interactive)
                                         (w32-browser
                                          (dired-replace-in-string
                                           "/" "\\"
                                           (dired-get-filename)))))))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "U" 'dired-up-directory)
            (define-key dired-mode-map "/" 'dired-isearch-filenames)))

;;sort setting
(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (make-local-variable  'dired-sort-map)
            (setq dired-sort-map (make-sparse-keymap))
            (define-key dired-mode-map "s" dired-sort-map)
            (define-key dired-sort-map "s"                ;; s s 
              (lambda () "sort by Size"
                      (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
            (define-key dired-sort-map "x"                ;; s x 
              (lambda () "sort by eXtension"
                      (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
            (define-key dired-sort-map "t"                ;; s t 
              (lambda () "sort by Time"
                      (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
            (define-key dired-sort-map "n"                ;; s n 
              (lambda () "sort by Name"
                      (interactive) (dired-sort-other (concat dired-listing-switches ""))))
            ))

;;dir first
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'sof/dired-sort)


;;filter files
;(add-hook 'dired-mode-hook
;          (lambda ()
;            (interactive)
;            (define-key dired-mode-map (kbd "/")  'dired-omit-expunge)))     

(setq dired-guess-shell-alist-user
      (list
       (list "\\.chm$"  "xchm")
       (list "\\.rm$"   "vlc")
       (list "\\.rmvb$" "vlc")
       (list "\\.avi$"  "vlc")
       (list "\\.asf$"  "vlc")
       (list "\\.wma$"  "vlc")
       (list "\\.htm$"  "firefox")
       (list "\\.html$" "firefox")
       (list "\\.mp3$"  "vlc")
       (list "\\.pdf$"  "evince")
       (list "\\.ppt$"  "soffice")
       (list "\\.docx$" "soffice")
       (list "\\.doc$"  "soffice")
       ))

(provide 'dired-setting)

;;; dired-setting.el ends here
