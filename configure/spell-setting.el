;;;; spell-setting.el --- spell config file
;;

;;https://joelkuiper.eu/spellcheck_emacs
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))


(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-hunspell t))


(provide 'spell-setting)

;;; spell-setting.el ends here
