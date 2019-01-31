;;;; fkey-setting.el --- key config file
;;;

(zz-load-path "elisp")
(require 'apply-keys)

(apply-keys-to-map
 ctrl-z-map
 (list
  (kbd "1")       (lookup-key global-map [f1])
  (kbd "2")       (lookup-key global-map [f2])
  (kbd "3")       (lookup-key global-map [f3])
  (kbd "4")       (lookup-key f4-map     [f4])
  (kbd "5")       (lookup-key global-map [f5])
  (kbd "6")       (lookup-key global-map [f6])
  (kbd "7")       (lookup-key global-map [f7])
  (kbd "8")       (lookup-key global-map [f8])
  (kbd "9")       (lookup-key global-map [f9])
  (kbd "0")       (lookup-key global-map [f10])
  (kbd "-")       (lookup-key global-map [f11])
  (kbd "=")       (lookup-key global-map [f12])

  (kbd "!")       (lookup-key global-map [S-f1])
  (kbd "@")       (lookup-key global-map [S-f2])
  (kbd "#")       (lookup-key global-map [S-f3])
  (kbd "$")       (lookup-key global-map [S-f4])
  (kbd "%")       (lookup-key global-map [S-f5])
  (kbd "^")       (lookup-key global-map [S-f6])
  (kbd "&")       (lookup-key global-map [S-f7])
  (kbd "*")       (lookup-key global-map [S-f8])
  (kbd "(")       (lookup-key global-map [S-f9])
  (kbd ")")       (lookup-key global-map [S-f10])
  (kbd "_")       (lookup-key global-map [S-f11])
  (kbd "+")       (lookup-key global-map [S-f12])

  (kbd "C-1")     (lookup-key global-map [C-f1])
  (kbd "C-2")     (lookup-key global-map [C-f2])
  (kbd "C-3")     (lookup-key global-map [C-f3])
  (kbd "C-4")     (lookup-key global-map [C-f4])
  (kbd "C-5")     (lookup-key global-map [C-f5])
  (kbd "C-6")     (lookup-key global-map [C-f6])
  (kbd "C-7")     (lookup-key global-map [C-f7])
  (kbd "C-8")     (lookup-key global-map [C-f8])
  (kbd "C-9")     (lookup-key global-map [C-f9])
  (kbd "C-0")     (lookup-key global-map [C-f10])
  (kbd "C--")     (lookup-key global-map [C-f11])
  (kbd "C-=")     (lookup-key global-map [C-f12])

  (kbd "M-1")     (lookup-key global-map [M-f1])
  (kbd "M-2")     (lookup-key global-map [M-f2])
  (kbd "M-3")     (lookup-key global-map [M-f3])
  (kbd "M-4")     (lookup-key global-map [M-f4])
  (kbd "M-5")     (lookup-key global-map [M-f5])
  (kbd "M-6")     (lookup-key global-map [M-f6])
  (kbd "M-7")     (lookup-key global-map [M-f7])
  (kbd "M-8")     (lookup-key global-map [M-f8])
  (kbd "M-9")     (lookup-key global-map [M-f9])
  (kbd "M-0")     (lookup-key global-map [M-f10])
  (kbd "M--")     (lookup-key global-map [M-f11])
  (kbd "M-=")     (lookup-key global-map [M-f12])
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-M-1")   (lookup-key global-map [C-f1])
  (kbd "C-M-2")   (lookup-key global-map [C-f2])
  (kbd "C-M-3")   (lookup-key global-map [C-f3])
  (kbd "C-M-4")   (lookup-key global-map [C-f4])
  (kbd "C-M-5")   (lookup-key global-map [C-f5])
  (kbd "C-M-6")   (lookup-key global-map [C-f6])
  (kbd "C-M-7")   (lookup-key global-map [C-f7])
  (kbd "C-M-8")   (lookup-key global-map [C-f8])
  (kbd "C-M-9")   (lookup-key global-map [C-f9])
  (kbd "C-M-0")   (lookup-key global-map [C-f10])
  (kbd "C-M--")   (lookup-key global-map [C-f11])
  (kbd "C-M-=")   (lookup-key global-map [C-f12])
  ))

(apply-keys-to-map
 ctrl-z-map
 (list
  (kbd "B")       (lookup-key global-map [S-left])
  (kbd "F")       (lookup-key global-map [S-right])
  (kbd "N")       (lookup-key global-map [S-down])
  (kbd "P")       (lookup-key global-map [S-up])

  (kbd "C-b")     (lookup-key global-map [C-left])
  (kbd "C-f")     (lookup-key global-map [C-right])
  (kbd "C-n")     (lookup-key global-map [C-down])
  (kbd "C-p")     (lookup-key global-map [C-up])

  (kbd "M-b")     (lookup-key global-map [M-left])
  (kbd "M-f")     (lookup-key global-map [M-right])
  (kbd "M-n")     (lookup-key global-map [M-down])
  (kbd "M-p")     (lookup-key global-map [M-up])
  ))

(apply-keys-to-map
 fn-map
 (list
  ;;quick move other windows
  (kbd "p")       (lookup-key global-map [up])
  (kbd "n")       (lookup-key global-map [down])
  (kbd "b")       (lookup-key global-map [left])
  (kbd "f")       (lookup-key global-map [right])
  ))

(apply-keys-to-map
 meta-fn-map
 (list
  ;;quick move other windows
  (kbd "p")       (lookup-key global-map [M-up])
  (kbd "n")       (lookup-key global-map [M-down])
  (kbd "b")       (lookup-key global-map [M-left])
  (kbd "f")       (lookup-key global-map [M-right])
  ))

(apply-keys-to-map
 shift-fn-map
 (list
  ;;window size change
  (kbd "p")       (lookup-key global-map [S-up])
  (kbd "n")       (lookup-key global-map [S-down])
  (kbd "b")       (lookup-key global-map [S-left])
  (kbd "f")       (lookup-key global-map [S-right])
  ))

(apply-keys-to-map
 ctrl-fn-map
 (list
  (kbd "p")       (lookup-key global-map [C-up])
  (kbd "n")       (lookup-key global-map [C-down])
  (kbd "b")       (lookup-key global-map [C-left])
  (kbd "f")       (lookup-key global-map [C-right])
  ))


(provide 'fkey-setting)

;;; fkey-setting.el ends here
