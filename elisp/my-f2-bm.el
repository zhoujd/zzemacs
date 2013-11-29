;;write by zhoujd for f2 functions

;;my f2 mark setting
(setq my-f2-idx     ?0
      my-f2-jmp-idx ?0
      my-f2-max     ?0)

(defun my-f2-setup ()
  (interactive)
  (when (= 0 (length (buffer-file-name (current-buffer))))
    (message "this buffer is not a file buffer"))
  (unless (= 0 (length (buffer-file-name (current-buffer))))
    (if (> my-f2-idx ?9)
        (progn
          (setq my-f2-idx ?0)
          (set-register my-f2-idx
           (list 'file-query (buffer-file-name (current-buffer)) (point))))
        (progn
          (set-register my-f2-idx
           (list 'file-query (buffer-file-name (current-buffer)) (point)))))
    (message "my-f2-setup %s" (string  my-f2-idx))
    (if (< my-f2-max my-f2-idx) (setq my-f2-max my-f2-idx))
    (setq my-f2-idx (+ 1 my-f2-idx))))

(defun my-f2-jmp-down ()
  (interactive)
  (when (> my-f2-jmp-idx my-f2-max)
        (setq my-f2-jmp-idx ?0))
  (jump-to-register my-f2-jmp-idx)
  (message "my-f2-jmp-down %s" (string my-f2-jmp-idx))
  (setq my-f2-jmp-idx (+ 1 my-f2-jmp-idx)))

(defun my-f2-jmp-up ()
  (interactive)
  (when (< my-f2-jmp-idx ?0)
        (setq my-f2-jmp-idx my-f2-max))
  (jump-to-register my-f2-jmp-idx)
  (message "my-f2-jmp-up %s" (string my-f2-jmp-idx))
  (setq my-f2-jmp-idx (- my-f2-jmp-idx 1)))

(defun clear-f2 ()
  (interactive)
  (setq my-f2-idx     ?0
        my-f2-jmp-idx ?0
        my-f2-max     ?0)
  (setq register-alist nil))

;;key bind like this 
(global-set-key [C-f2] 'my-f2-setup)
(global-set-key [f2] 'my-f2-jmp-down)
(global-set-key [S-f2] 'my-f2-jmp-up)
(global-set-key [M-f2] 'clear-f2)

(provide 'my-f2-bm)

