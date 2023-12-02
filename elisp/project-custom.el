;;; project custom

;;develop setting for tags path etc.
(defvar zz:dev-set-file (format "%s/.emacs.d/temp-setting.el"
                                (getenv "HOME"))
  "temp project setting")

;;generate temp-setting.el
(defun zz:create-file (fpath content)
  "Process the file at path FPATH ..."
  (let ((tmp-buf-name (file-name-nondirectory fpath)))
    (set-buffer (get-buffer-create tmp-buf-name))
    (goto-char 0)
    (dolist (item content)
      (insert item)
      (insert "\n"))
    (write-file fpath)
    (kill-buffer tmp-buf-name)))

(defvar zz:proj-list (list zzemacs-path)
  "project directory list")

;;temp setting template
(defconst zz:temp-template
  '(
    ";;;; temp-setting.el --- program temp file"
    ""
    ";; set project direcitory list"
    "(setq zz:proj-list '("
    "                     \"/usr/include\""
    "                     \"~/work/project-1\""
    "                     \"~/work/project-2\""
    "                     ))"
    ""
     "(mapc #'zz:add-os-path"
    "      '("
    "        \"~/work/script\""
    "        ))"
    ""
    "(mapc #'zz:add-lib-path"
    "      '("
    "        \"~/work/lib\""
    "        ))"
    ""
    ";; project key setting"
    ";(zz:exec-key f4-p-map \"f\" zz:firefox '(\"firefox\" \"http://www.baidu.com\"))"
    ))

(defun zz:temp-setting ()
  "Create ~/.emacs.d/temp-setting.el"
  (interactive)
  (let ((path zz:dev-set-file))
    (if (file-exists-p path)
        (progn
         (find-file path)
         (message "open %s successful." path))
        (progn
         (zz:create-file path zz:temp-template)
         (message "create %s successful." path))
      )))

(defun zz:temp-delete ()
  "delete ~/.emacs.d/temp-setting.el"
  (interactive)
  (let ((path zz:dev-set-file))
    (when (file-exists-p path)
      (delete-file path)
      (message "delete %s successful." path))
      ))
;;creast etags/cscope for multi project
(defun zz:gen-proj-find-path (proj-list)
  (let ((proj-path-parts ""))
    (dolist (cell proj-list)
      (setq proj-path-parts (concat proj-path-parts cell " ")))
    (substring proj-path-parts 0 -1)))

(defun zz:create-proj-etags ()
  (interactive)
  (zz:create-etags (zz:gen-proj-find-path zz:proj-list)))

(defun zz:create-proj-cscope ()
  (interactive)
  (zz:create-cscope (zz:gen-proj-find-path zz:proj-list)))

(defvar zz:tag-root (format "%s/.emacs.d/tags/"
                            (getenv "HOME"))
  "tag directory root")

;;update root tag path
(defun zz:update-root-tag-path ()
  (let ((tag-path (concat zz:tag-root "TAGS")))
    (when (and
           (file-exists-p tag-path)
           (not (member tag-path tags-table-list)))
      (push tag-path tags-table-list))))

;;init root tag
(if (file-exists-p zz:tag-root)
    (zz:update-root-tag-path)
    (make-directory zz:tag-root))

;;create etags to zz:tag-root
(defun zz:create-root-etags ()
  (interactive)
  (let ((default-directory zz:tag-root))
    (unless (file-exists-p zz:tag-root)
      (make-directory zz:tag-root))
    (zz:create-proj-etags)
    (zz:update-root-tag-path)))

;;create cscope to zz:tag-root
(defun zz:create-root-cscope ()
  (interactive)
  (let ((default-directory zz:tag-root))
    (unless (file-exists-p zz:tag-root)
      (make-directory zz:tag-root))
    (zz:create-proj-cscope)))

(defun zz:rscope-autoinit-path (buffer)
  "Look the directory from zz:tag-root"
  (when (file-readable-p (concat zz:tag-root rscope-database-name))
    zz:tag-root))

;;set cscope tag root
(defun zz:set-root-cscope ()
  (interactive)
  (cscope-set-initial-directory zz:tag-root)
  (setq helm-cscope-search-dir-init (list (list zz:tag-root)))
  (add-hook 'rscope-autoinit-cscope-dir-hooks
            (function zz:rscope-autoinit-path))
  (message "Set cscope init directory: %s" zz:tag-root))

;;unset cscope tag root
(defun zz:unset-root-cscope ()
  (interactive)
  (cscope-unset-initial-directory)
  (setq helm-cscope-search-dir-init nil)
  (remove-hook 'rscope-autoinit-cscope-dir-hooks
               (function zz:rscope-autoinit-path))
  (message "Unset cscope init directory"))

;;load temp setting
(when (file-exists-p zz:dev-set-file)
  (load-file zz:dev-set-file))


(provide 'project-custom)

;;; project-custom.el ends here
