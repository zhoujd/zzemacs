;;; dired-play.el

(require 'dired-aux)

(defvar dired-filelist-cmd '(("vlc" "-L")))
(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd
     nil
     shell-file-name
     shell-command-switch
     (format
      "nohup 1>/dev/null 2>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
          cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

(define-key dired-mode-map "r" 'dired-start-process)


(provide 'dired-play)

;;; end of dired-play.el
