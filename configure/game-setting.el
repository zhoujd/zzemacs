;;;; game-setting.el --- game setting file
;;;


(zz-load-path "site-lisp/games")

(setq sokoban-level-file (concat zzemacs-path "/etc/" "sokoban.levels"))
(require 'sokoban)

(provide 'game-setting)

;;; game-setting.el ends here
