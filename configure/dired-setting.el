;;;; dired-setting.el --- dired config file
;;;

(require 'dired-x)

;;dired setting
(setq dired-recursive-deletes t)
(setq dired-recursive-copies t)
(defadvice dired-find-file (around dired-find-file-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-file-for-visit)))
    ad-do-it
    (when (and (file-directory-p filename)
               (not (eq (current-buffer) orig)))
      (kill-buffer orig))))
(defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer)))
    ad-do-it
    (kill-buffer orig)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "U" 'dired-up-directory)
            (define-key dired-mode-map "/" 'dired-isearch-filenames)))

;;sort setting
(add-hook 'dired-mode-hook (lambda ()
  (interactive)
  (make-local-variable  'dired-sort-map)
  (setq dired-sort-map (make-sparse-keymap))
  (define-key dired-mode-map "s" dired-sort-map)
  (define-key dired-sort-map "s"                ;; s s 按照文件大小排序。
              '(lambda () "sort by Size"
                (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
  (define-key dired-sort-map "x"                ;; s x 按照文件扩展名排序。
              '(lambda () "sort by eXtension"
                 (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
  (define-key dired-sort-map "t"                ;; s t 按照文件访问时间排序。
              '(lambda () "sort by Time"
                 (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
  (define-key dired-sort-map "n"                ;; s n 按照文件名称的字母顺序排序。
              '(lambda () "sort by Name"
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


;;      过滤文件
;   |(add-hook 'dired-mode-hook (lambda ()
;   |  (interactive)
;   |  (define-key dired-mode-map (kbd "/")  'dired-omit-expunge)))     

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
       )
      ) 

(provide 'dired-setting)

;;; dired-setting.el ends here
