;;; define keys

;;multi key setting for one map
(defun apply-keys-to-map (map key-pairs)
  "apply multi key defines"
  (let ((i 0))
    (while (< i (length key-pairs))
      (let ((key (nth i key-pairs))
            (fn (nth (1+ i) key-pairs)))
        (when fn
          (define-key map key fn)))
      (setq i (+ i 2)))))

;;multi key setting for multi maps
(defun apply-keys-to-maps (maps key-pairs)
  "apply multi key defines"
  (dolist (map maps)
    (let ((i 0))
      (while (< i (length key-pairs))
        (let ((key (nth i key-pairs))
              (fn (nth (1+ i) key-pairs)))
          (when fn
            (define-key map key fn)))
        (setq i (+ i 2))))))


(provide 'apply-keys)

;;; apply-keys.el ends here
