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
  (start-process "urxvt"
                 nil
                 "urxvt"
                 "-cd"
                 default-directory))

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

;;go to last frame
(defvar zz:switch-frame-flag t)
(defun zz:last-frame-go ()
  (interactive)
  (if zz:switch-frame-flag
      (other-frame +1)
      (other-frame -1))
  (setq zz:switch-frame-flag (not zz:switch-frame-flag)))

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
    ))

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

;;edit current file with sudo
(defun zz:prepare-tramp-sudo-string (tempfile)
  (if (file-remote-p tempfile)
      (let* ((parts (s-split ":" tempfile))
             (hostname (nth 1 parts))
             (filepath (car (last parts))))
        (concat "/ssh" ":" hostname "|" "sudo" ":" hostname ":" filepath))
      (concat "/sudo:root@localhost:" tempfile)))

(defun zz:sudo-edit-current-file ()
  (interactive)
  (let ((my-file-name) ; fill this with the file to open
        (position))    ; if the file is already open save position
    (if (equal major-mode 'dired-mode) ; test if we are in dired-mode
        (progn
          (setq my-file-name (dired-get-file-for-visit))
          (find-alternate-file (zz:prepare-tramp-sudo-string my-file-name)))
        (progn
          (setq my-file-name (buffer-file-name)
                position (point))
          (find-alternate-file (zz:prepare-tramp-sudo-string my-file-name))
          (goto-char position)))))

(defun zz:path ()
  (interactive)
  (message (buffer-file-name)))

(defun zz:kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun zz:delete-process ()
  (interactive)
  (let ((pname (ido-completing-read "Process Name: "
                                    (mapcar 'process-name (process-list)))))

    (delete-process (get-process pname))))

;;also handle undocumented (<active> <inactive>) form
(defun zz:transparency-toggle ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 90) '(100 . 100)))))

(defun zz:transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun zz:python-scratch ()
  (interactive)
    (let (
          ;; Creates a new buffer object.
         (buf (get-buffer-create "*python-scratch*"))
         )
      ;; Executes functions that would change the current buffer at
      ;; buffer buf
     (with-current-buffer buf
       ;;; Set the new buffer to scratch mode
       (python-mode)
       ;;; Pop to scratch buffer
       (pop-to-buffer buf)
       )))

(defun zz:sh-scratch ()
  (interactive)
    (let (
          ;; Creates a new buffer object.
         (buf (get-buffer-create "*sh-scratch*"))
         )
      ;; Executes functions that would change the current buffer at
      ;; buffer buf
     (with-current-buffer buf
       ;;; Set the new buffer to scratch mode
       (sh-mode)
       ;;; Pop to scratch buffer
       (pop-to-buffer buf)
       )))

(defun zz:reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs"))

(defun zz:reopen-remote-file-as-root ()
  "Reopen a remote file as root over tramp."
  (find-alternate-file
   (let* ((parts (s-split ":" buffer-file-name))
          (hostname (nth 1 parts))
          (filepath (car (last parts))))
     (concat "/ssh" ":" hostname "|" "sudo" ":" hostname ":" filepath))))

(defun zz:reopen-file-as-root ()
  "Reopen a local or remote file as root."
  (interactive)
  (if (file-remote-p default-directory)
      (zz:reopen-remote-file-as-root)
      (crux-reopen-as-root)))

;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
(defun zz:display-current-time ()
  (interactive)
  (message (format-time-string "%a %b %e %Y WW%U %l:%M %p")))

;;https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun zz:sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))


;; Source: https://www.emacswiki.org/emacs/misc-cmds.el
;; M-x: revert-buffer-with-coding-system <C-x RET r>
(defun zz:revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))


(provide 'sample-setting)

;;; sample-setting.el ends here
