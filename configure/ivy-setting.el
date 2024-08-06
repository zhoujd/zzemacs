;;;; ivy-setting.el --- ivy configure file

(zz:load-path "site-lisp/ivy")
(require 'ivy)

(defun zz:ivy-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs
         (delete-dups
          (mapcar (lambda (file)
                    (if (file-directory-p file)
                        file
                        (file-name-directory file)))
                  recentf-list))))
    (let ((dir (ivy-read "Directory: "
                         recent-dirs
                         :re-builder #'ivy--regex
                         :sort nil
                         :initial-input nil)))
      (dired dir))))


(provide 'ivy-setting)

;;;; ivy-setting.el --- end here
