;;;; game-setting.el --- game setting file
;;;

;;sokoban setting
(zz-load-path "site-lisp/sokoban")
(setq sokoban-level-file (concat zzemacs-path "/site-lisp/sokoban/sokoban.levels"))
(require 'sokoban)

(provide 'game-setting)

;;; game-setting.el ends here
