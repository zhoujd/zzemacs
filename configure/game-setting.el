;;;; game-setting.el --- game setting file
;;;

(zz-load-path "site-lisp/game")

;;sokoban setting
(setq sokoban-level-file (concat zzemacs-path "/site-lisp/game/sokoban.levels"))
(require 'sokoban)

;;auto play tetris
(require 'autotetris-mode)


(provide 'game-setting)

;;; game-setting.el ends here
