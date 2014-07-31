;;;;sample-file-process

;; idiom for processing a list of files in dired's marked fileas
;; suppose myProcessFile is your function that takes a file path
;; and do some processing on the file
;; to use it, type M-x dired-myProcessFile
(defun dired-myProcessFile ()
  "apply myProcessFile function to marked files in dired."
  (interactive)
  (require 'dired)
  (mapc 'myProcessFile (dired-get-marked-files)))

(defun myProcessFile (fpath)
  "Process the file at path FPATH ..."
  (let ((temp-buf nil)
        (temp-buf-name "myTemp"))
    ;; create temp buffer without undo record or font lock. (more efficient)
    ;; first space in temp buff name is necessary
    (setq temp-buf (set-buffer (get-buffer-create temp-buf-name)))
    (insert-file-contents fpath nil nil nil t)

    ;; process it ...
    ;; (goto-char 0) ; move to begining of file's content (in case it was open)
    ;; ... do something here
    (whitespace-cleanup)
    
    (write-file fpath) ;; write back to the file
    (kill-buffer temp-buf)
    ))

(provide 'sample-file-process)

;;;sample-file-process end
