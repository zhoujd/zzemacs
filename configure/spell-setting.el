;;;; spell-setting.el --- spell config file
;;

;;https://joelkuiper.eu/spellcheck_emacs
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))


(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-hunspell t))

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.
Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.
All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
        (if (derived-mode-p 'prog-mode)
            (progn
              (message "Flyspell on (code)")
              (flyspell-prog-mode))
            ;; else
            (progn
              (message "Flyspell on (text)")
              (flyspell-mode 1)))
        ;; I tried putting (flyspell-buffer) here but it didn't seem to work
        )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.
When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ;; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
      ;; else - flyspell is off, turn it on
      (flyspell-on-for-buffer-type)))


(provide 'spell-setting)

;;; spell-setting.el ends here
