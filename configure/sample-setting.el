;;;; sample-setting.el --- sample config file
;;;

;;my unicad enable/disable switch
(defun zz-unicad-switch ()
  "unicad enable/disable switch"
  (interactive)
  (if (eq t unicad-global-enable)
    (progn (setq unicad-global-enable nil)
           (message "unicad is disabled"))
    (progn (setq unicad-global-enable t)
           (message "unicad is enabled"))))

;;using spaces repalce tabs
(defun untabify-buffer ()
  "untabify whole buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (save-buffer))

;;using tabs repalce spaces
(defun tabify-buffer ()
  "tabify whole buffer"
  (interactive)
  (tabify (point-min) (point-max))
  (save-buffer))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;;File coding for Unix Dos Mac switcher
(defvar zz-os-flag 0)
(defun zz-os-file-switch ()
    "for unix dos max"
    (interactive)
    (if (> zz-os-flag 2)
      (setq zz-os-flag 0))
    (cond ((eq zz-os-flag 0)
           (set-buffer-file-coding-system 'unix 't)
           (message "end line with LF"))
          ((eq zz-os-flag 1)
           (set-buffer-file-coding-system 'dos 't)
           (message "end line with CRLF"))
          ((eq zz-os-flag 2)
           (set-buffer-file-coding-system 'mac 't)
           (message "end line with CR")))
    (setq zz-os-flag (+ zz-os-flag 1)))

;;; Maximum Windows Frame
(defvar zz-fullscreen-p t "Check if fullscreen is on or off")
(defun zz-sub-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                           'fullboth)))

(defun zz-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
      (run-with-idle-timer 0.1 nil 'zz-sub-fullscreen)))

(defun zz-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
      (run-with-idle-timer 0.1 nil 'zz-sub-fullscreen)))

(defun zz-toggle-fullscreen ()
  (interactive)
  (setq zz-fullscreen-p (not zz-fullscreen-p))
  (if zz-fullscreen-p
	  (zz-non-fullscreen)
	(zz-fullscreen)))

(defvar zz-maxframe-p t "Check if maxframe is on or off")
(defun zz-toggle-maxframe ()
  (interactive)
  (setq zz-maxframe-p (not zz-maxframe-p))
  (if zz-maxframe-p
	  (restore-frame)
	(maximize-frame)))

;;display current buffer name
(defun display-buffer-name ()
  (interactive)
  (message (buffer-file-name (current-buffer))))

;;set file to utf-8
(defun zz-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer)
  (message "this file to utf-8 ok!"))

(defun switch-to-scratch ()
  "switch to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  (message "switch to *scratch*"))

(defun switch-to-compilation ()
  "switch to *compilation* buffer"
  (interactive)
  (switch-to-buffer "*compilation*")
  (message "switch to *compilation*"))

(defun edit-with-gvim()
  "Edit current buffer file with gvim"
  (interactive)
  (start-process  "gvim"
                  nil
                  "gvim"
                  (display-buffer-name)))


(defun open-with-terminal()
  "Open terminal to current dired"
  (interactive)
  (start-process  "gnome-terminal"
                  nil
                  "gnome-terminal"
                  ""
                  ))

(defun open-with-nautilus()
  "Open nautilus on current dired"
  (interactive)
  (start-process  "nautilus"
                  nil
                  "nautilus"
                  "--no-desktop"
                  ""
                  ))

;;go to last buffer
(defun zz-last-buffer-go ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun line-to-top-of-window ()
    (interactive)
    (recenter 0))

;; used in my ion C-F2 key binding. run before shutting down X!
(defun delete-all-x-frames ()
  (mapcar (lambda (frame) (if (eq 'x (frame-live-p frame))
                              (delete-frame frame)))
          (frame-list)))

(defun make-zz-frames ()
  (interactive)
  (make-frame '((name . "emacs main")))
  (make-frame '((name . "emacs secondary")))
  )

;;file transform
(defun zz-dos2unix (buffer)
  "Automate M-% C-q C-m RET RET !" 
  (interactive "*b") 
  (save-excursion 
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(defun dos2unix ()
  "Automate M-% C-q C-m RET RET !"
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun unix2dos ()
  "Automate M-% C-q C-j RET C-q C-m C-q C-j RET !"
  (interactive)
  (set-buffer-file-coding-system 'dos))

;;occur setting
(defun zz-occur (&optional arg)
  "Switch to *Occur* buffer, or run `occur'.
   Without a prefix argument, switch to the buffer.
   With a universal prefix argument, run occur again.
   With a numeric prefix argument, run occur with NLINES
   set to that number."
  (interactive "P")
  (if (and (not arg) (get-buffer "*Occur*"))
      (switch-to-buffer "*Occur*")
      (occur (completing-read "Search Term: " nil nil nil (thing-at-point 'word))
             (if (listp arg) 0 arg))))

;;emacs with sudo
(defun zz-sudo-find-file (file dir)
  (find-file (concat "/sudo:localhost:" (expand-file-name file dir))))

;;open special info file
(defun zz-info-open-file (dir-name)
  "Create tags file."
  (interactive "FInfo file: ")
  (info dir-name))


(provide 'sample-setting)

;;; sample-setting.el ends here
