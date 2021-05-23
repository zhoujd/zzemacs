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

;;multi keys unset for one map
(defun keys-unset-to-map (map keys)
  "multi keys unset defines"
  (dolist (key keys)
    (define-key map key nil)))


(provide 'apply-keys)

;;; apply-keys.el ends here
