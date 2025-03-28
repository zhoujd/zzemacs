;;; define keys

(defun apply-keys-to-map (map key-pairs)
  "apply keys for one map"
  (let ((i 0))
    (while (< i (length key-pairs))
      (let ((key (nth i key-pairs))
            (fn (nth (1+ i) key-pairs)))
        (when fn
          (define-key map key fn)))
      (setq i (+ i 2)))))

(defun apply-keys-to-maps (maps key-pairs)
  "apply keys for multi maps"
  (dolist (map maps)
    (apply-keys-to-map map key-pairs)))

(defun unset-keys-to-map (map keys)
  "unset keys for one map"
  (dolist (key keys)
    (define-key map key nil)))

(defun unset-keys-to-maps (maps keys)
  "unset keys for multi maps"
  (dolist (map maps)
    (unset-keys-to-map map keys)))

(defvar fn-key-table
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "f1"  "1" hash)
    (puthash "f2"  "2" hash)
    (puthash "f3"  "3" hash)
    (puthash "f4"  "4" hash)
    (puthash "f5"  "5" hash)
    (puthash "f6"  "6" hash)
    (puthash "f7"  "7" hash)
    (puthash "f8"  "8" hash)
    (puthash "f9"  "9" hash)
    (puthash "f10" "0" hash)
    (puthash "f11" "-" hash)
    (puthash "f12" "=" hash)
    hash)
  "fn-key-table")

(defun add-fn-key (name map sym func)
  "add function key"
  (when (and name map sym func)
    (define-key global-map sym  func)
    (define-key map        name func)
    (unless (keymapp func)
      (define-key help-fn-map sym func))))

(defun define-fn-key (fn-name fn-sym fn s-fn-sym s-fn
                              c-fn-sym c-fn m-fn-sym m-fn
                              ctrl-x-fn-sym ctrl-x-fn
                              ctrl-c-fn-sym ctrl-c-fn &optional doc)
  "define function keys"
  (add-fn-key fn-name fn-map        fn-sym fn)
  (add-fn-key fn-name shift-fn-map  s-fn-sym s-fn)
  (add-fn-key fn-name ctrl-fn-map   c-fn-sym c-fn)
  (add-fn-key fn-name meta-fn-map   m-fn-sym m-fn)
  (add-fn-key fn-name ctrl-x-fn-map ctrl-x-fn-sym ctrl-x-fn)
  (add-fn-key fn-name ctrl-c-fn-map ctrl-c-fn-sym ctrl-c-fn))


(provide 'apply-keys)

;;; apply-keys.el ends here
