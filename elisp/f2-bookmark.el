;;write by zhoujd for f2 functions

;;my f2 mark setting
(setq zz:f2-idx     ?0
      zz:f2-jmp-idx ?0
      zz:f2-max     ?0)

(defun zz:f2-setup ()
  (interactive)
  (when (= 0 (length (buffer-file-name (current-buffer))))
    (message "this buffer is not a file buffer"))
  (unless (= 0 (length (buffer-file-name (current-buffer))))
    (if (> zz:f2-idx ?9)
        (progn
          (setq zz:f2-idx ?0)
          (set-register zz:f2-idx
           (list 'file-query (buffer-file-name (current-buffer)) (point))))
        (progn
          (set-register zz:f2-idx
           (list 'file-query (buffer-file-name (current-buffer)) (point)))))
    (message "zz:f2-setup %s" (string  zz:f2-idx))
    (if (< zz:f2-max zz:f2-idx) (setq zz:f2-max zz:f2-idx))
    (setq zz:f2-idx (+ 1 zz:f2-idx))))

(defun zz:f2-jmp-down ()
  (interactive)
  (when (> zz:f2-jmp-idx zz:f2-max)
        (setq zz:f2-jmp-idx ?0))
  (jump-to-register zz:f2-jmp-idx)
  (message "zz:f2-jmp-down %s" (string zz:f2-jmp-idx))
  (setq zz:f2-jmp-idx (+ 1 zz:f2-jmp-idx)))

(defun zz:f2-jmp-up ()
  (interactive)
  (when (< zz:f2-jmp-idx ?0)
        (setq zz:f2-jmp-idx zz:f2-max))
  (jump-to-register zz:f2-jmp-idx)
  (message "zz:f2-jmp-up %s" (string zz:f2-jmp-idx))
  (setq zz:f2-jmp-idx (- zz:f2-jmp-idx 1)))

(defun zz:f2-clear ()
  (interactive)
  (setq zz:f2-idx     ?0
        zz:f2-jmp-idx ?0
        zz:f2-max     ?0)
  (setq register-alist nil))

;;key bind like this 
(global-set-key [C-f2] 'zz:f2-setup)
(global-set-key [f2]   'zz:f2-jmp-down)
(global-set-key [S-f2] 'zz:f2-jmp-up)
(global-set-key [M-f2] 'zz:f2-clear)

(provide 'self-bookmark)

;;; self-bookmark.el end here
