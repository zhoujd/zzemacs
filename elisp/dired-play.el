;;; dired-play.el
;;
;;(require 'dired-play)
;;(define-key dired-mode-map "r" 'dired-play-start)
;;(define-key dired-mode-map "z" 'dired-play-stop)

(require 'dired-aux)

;;filelist cmd
(defvar dired-filelist-cmd '(("vlc" "-L")))
(defvar dired-play-prefix "nohup 1>/dev/null 2>/dev/null")

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

;;play files which marked
(defun dired-play-start (cmd &optional file-list)
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
     (format "%s %s \"%s\""
             dired-play-prefix
             (if (and (> (length file-list) 1)
                      (setq list-switch
                            (cadr (assoc cmd dired-filelist-cmd))))
                 (format "%s %s" cmd list-switch)
                 cmd)
             (mapconcat #'expand-file-name
                        (mapcar (lambda (file)
                                  (replace-in-string "\"" "\\\"" file))
                                file-list)
                        "\" \"")))))

;;delete play process vlc or mpv
(defun dired-play-stop ()
  (interactive)
  (let* ((plist '("vlc" "cvlc" "vlca" "mpv" "mpa"))
         (pname (ido-completing-read "Process Name: " (push "all" plist))))
    (if (string= pname "all")
        (dolist (pitem plist)
          (when (get-process pitem)
            (delete-process (get-process pitem))))
        (when (get-process pname)
          (delete-process (get-process pname))))))


(provide 'dired-play)

;;; end of dired-play.el
