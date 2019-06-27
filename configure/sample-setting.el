;;;; sample-setting.el --- sample config file
;;;

;;my unicad enable/disable switch
(defun zz:unicad-switch ()
  "unicad enable/disable switch"
  (interactive)
  (if (eq t unicad-global-enable)
    (progn (setq unicad-global-enable nil)
           (message "unicad is disabled"))
    (progn (setq unicad-global-enable t)
           (message "unicad is enabled"))))

;;using spaces repalce tabs
(defun zz:untabify-buffer ()
  "untabify whole buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (save-buffer))

;;using tabs repalce spaces
(defun zz:tabify-buffer ()
  "tabify whole buffer"
  (interactive)
  (tabify (point-min) (point-max))
  (save-buffer))

(defun zz:cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (zz:untabify-buffer)
  (delete-trailing-whitespace))

;;File coding for Unix Dos Mac switcher
(defvar zz:os-flag 0)
(defun zz:os-file-switch ()
    "for unix dos max"
    (interactive)
    (if (> zz:os-flag 2)
      (setq zz:os-flag 0))
    (cond ((eq zz:os-flag 0)
           (set-buffer-file-coding-system 'unix 't)
           (message "end line with LF"))
          ((eq zz:os-flag 1)
           (set-buffer-file-coding-system 'dos 't)
           (message "end line with CRLF"))
          ((eq zz:os-flag 2)
           (set-buffer-file-coding-system 'mac 't)
           (message "end line with CR")))
    (setq zz:os-flag (+ zz:os-flag 1)))

;;; Maximum Windows Frame
(defvar zz:fullscreen-p t "Check if fullscreen is on or off")
(defun zz:sub-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                           'fullboth)))

(defun zz:non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
      (run-with-idle-timer 0.1 nil 'zz:sub-fullscreen)))

(defun zz:fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
      (run-with-idle-timer 0.1 nil 'zz:sub-fullscreen)))

(defun zz:toggle-fullscreen ()
  (interactive)
  (setq zz:fullscreen-p (not zz:fullscreen-p))
  (if zz:fullscreen-p
	  (zz:non-fullscreen)
	(zz:fullscreen)))

(defun zz:toggle-maxframe ()
  (interactive)
  (if-ms-windows
   (progn
    (w32-send-sys-command 61488))
   (progn
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
   ))

;;display current buffer name
(defun zz:display-buffer-name ()
  (interactive)
  (message (buffer-file-name (current-buffer))))

;;set file to utf-8
(defun zz:utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer)
  (message "this file to utf-8 ok!"))

(defun zz:switch-to-scratch ()
  "switch to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  (message "switch to *scratch*"))

(defun zz:switch-to-compilation ()
  "switch to *compilation* buffer"
  (interactive)
  (switch-to-buffer "*compilation*")
  (message "switch to *compilation*"))

(defun zz:open-with-gvim()
  "Edit current buffer file with gvim"
  (interactive)
  (start-process  "gvim"
                  nil
                  "gvim"
                  (zz:display-buffer-name)))

(defun zz:open-with-terminal()
  "Open terminal to current dired"
  (interactive)
  (start-process  "gnome-terminal"
                  nil
                  "gnome-terminal"
                  ""
                  ))

(defun zz:open-with-nautilus()
  "Open nautilus on current dired"
  (interactive)
  (start-process  "nautilus"
                  nil
                  "nautilus"
                  "--no-desktop"
                  ""
                  ))

;;go to last buffer
(defun zz:last-buffer-go ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun zz:line-to-top-of-window ()
    (interactive)
    (recenter 0))

;;run before shutting down X!
(defun zz:delete-all-x-frames ()
  (mapcar (lambda (frame)
            (if (eq 'x (frame-live-p frame))
                (delete-frame frame)))
          (frame-list)))

(defun zz:make-frames ()
  (interactive)
  (make-frame '((name . "emacs main")))
  (make-frame '((name . "emacs secondary"))))

;;file transform
(defun zz:dos2unix-1 (buffer)
  "Automate M-% C-q C-m RET RET !" 
  (interactive "*b") 
  (save-excursion 
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(defun zz:dos2unix ()
  "Automate M-% C-q C-m RET RET !"
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun zz:unix2dos ()
  "Automate M-% C-q C-j RET C-q C-m C-q C-j RET !"
  (interactive)
  (set-buffer-file-coding-system 'dos))

;;occur setting
(defun zz:occur-at-point (&optional arg)
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
(defun zz:sudo-find-file (file dir)
  (find-file (concat "/sudo:localhost:" (expand-file-name file dir))))

;;open special info file
(defun zz:open-info-file (dir-name)
  "Create tags file."
  (interactive "FInfo file: ")
  (info dir-name))

(defun zz:insert-numbers (arg)
  "insert number into a file, starting with 1   -mdf"
  (interactive "NHow many numbers to insert: ")
  (setq i 0)
  (while (< i arg)
    (setq i (1+ i))
    (insert (int-to-string i))
	(backward-char 1)
    (next-line 2)
    (beginning-of-line 0)
    )
  )

(defun zz:trans-path-sep (path origin-sep target-sep)
  (let ((tmp ""))
    (dolist (item (split-string path origin-sep))
      (setq tmp (concat tmp item target-sep)))
    (setq tmp (substring tmp 0 -2))
    tmp))

(defun zz:google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun zz:baidu ()
  "baidu the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.baidu.com/s?ie=utf-8&wd="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Baidu: "))))))

;;delete the current file
(defun zz:delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;rename the current file
(defun zz:rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
         (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

;;browse current HTML file
(defun zz:browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (tramp-tramp-file-p file-name)
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;enabling iimage-mode automatically
;;http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html
(defun zz:enable-iimage()
  "Enable iimage mode"
  (interactive)
  (turn-on-iimage-mode)
  (iimage-mode-buffer t))

;;refresh iimages
(defun zz:refresh-iimages ()
  "Only way I've found to refresh iimages (without also recentering)"
  (interactive)
  (clear-image-cache nil)
  (iimage-mode nil)
  (iimage-mode t)
  (message "Refreshed images"))


(provide 'sample-setting)

;;; sample-setting.el ends here
