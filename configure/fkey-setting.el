;;;; fkey-setting.el --- key config file
;;;

(zz:load-path "elisp")
(require 'apply-keys)

(apply-keys-to-map
 ctrl-z-map
 (list
  [f1]               (lookup-key global-map [f1])
  [f2]               (lookup-key global-map [f2])
  [f3]               (lookup-key global-map [f3])
  [f4]               (lookup-key global-map [f4])
  [f5]               (lookup-key global-map [f5])
  [f6]               (lookup-key global-map [f6])
  [f7]               (lookup-key global-map [f7])
  [f8]               (lookup-key global-map [f8])
  [f9]               (lookup-key global-map [f9])
  [f10]              (lookup-key global-map [f10])
  [f11]              (lookup-key global-map [f11])
  [f12]              (lookup-key global-map [f12])

  [S-f1]             (lookup-key global-map [S-f1])
  [S-f2]             (lookup-key global-map [S-f2])
  [S-f3]             (lookup-key global-map [S-f3])
  [S-f4]             (lookup-key global-map [S-f4])
  [S-f5]             (lookup-key global-map [S-f5])
  [S-f6]             (lookup-key global-map [S-f6])
  [S-f7]             (lookup-key global-map [S-f7])
  [S-f8]             (lookup-key global-map [S-f8])
  [S-f9]             (lookup-key global-map [S-f9])
  [S-f10]            (lookup-key global-map [S-f10])
  [S-f11]            (lookup-key global-map [S-f11])
  [S-f12]            (lookup-key global-map [S-f12])

  [C-f1]             (lookup-key global-map [C-f1])
  [C-f2]             (lookup-key global-map [C-f2])
  [C-f3]             (lookup-key global-map [C-f3])
  [C-f4]             (lookup-key global-map [C-f4])
  [C-f5]             (lookup-key global-map [C-f5])
  [C-f6]             (lookup-key global-map [C-f6])
  [C-f7]             (lookup-key global-map [C-f7])
  [C-f8]             (lookup-key global-map [C-f8])
  [C-f9]             (lookup-key global-map [C-f9])
  [C-f10]            (lookup-key global-map [C-f10])
  [C-f11]            (lookup-key global-map [C-f11])
  [C-f12]            (lookup-key global-map [C-f12])

  [M-f1]             (lookup-key global-map [M-f1])
  [M-f2]             (lookup-key global-map [M-f2])
  [M-f3]             (lookup-key global-map [M-f3])
  [M-f4]             (lookup-key global-map [M-f4])
  [M-f5]             (lookup-key global-map [M-f5])
  [M-f6]             (lookup-key global-map [M-f6])
  [M-f7]             (lookup-key global-map [M-f7])
  [M-f8]             (lookup-key global-map [M-f8])
  [M-f9]             (lookup-key global-map [M-f9])
  [M-f10]            (lookup-key global-map [M-f10])
  [M-f11]            (lookup-key global-map [M-f11])
  [M-f12]            (lookup-key global-map [M-f12])

  (kbd "C-x <f1>")   (lookup-key global-map (kbd "C-x <f1>"))
  (kbd "C-x <f2>")   (lookup-key global-map (kbd "C-x <f2>"))
  (kbd "C-x <f3>")   (lookup-key global-map (kbd "C-x <f3>"))
  (kbd "C-x <f4>")   (lookup-key global-map (kbd "C-x <f4>"))
  (kbd "C-x <f5>")   (lookup-key global-map (kbd "C-x <f5>"))
  (kbd "C-x <f6>")   (lookup-key global-map (kbd "C-x <f6>"))
  (kbd "C-x <f7>")   (lookup-key global-map (kbd "C-x <f7>"))
  (kbd "C-x <f8>")   (lookup-key global-map (kbd "C-x <f8>"))
  (kbd "C-x <f9>")   (lookup-key global-map (kbd "C-x <f9>"))
  (kbd "C-x <f10>")  (lookup-key global-map (kbd "C-x <f10>"))
  (kbd "C-x <f11>")  (lookup-key global-map (kbd "C-x <f11>"))
  (kbd "C-x <f12>")  (lookup-key global-map (kbd "C-x <f12>"))

  (kbd "C-c <f1>")   (lookup-key global-map (kbd "C-c <f1>"))
  (kbd "C-c <f2>")   (lookup-key global-map (kbd "C-c <f2>"))
  (kbd "C-c <f3>")   (lookup-key global-map (kbd "C-c <f3>"))
  (kbd "C-c <f4>")   (lookup-key global-map (kbd "C-c <f4>"))
  (kbd "C-c <f5>")   (lookup-key global-map (kbd "C-c <f5>"))
  (kbd "C-c <f6>")   (lookup-key global-map (kbd "C-c <f6>"))
  (kbd "C-c <f7>")   (lookup-key global-map (kbd "C-c <f7>"))
  (kbd "C-c <f8>")   (lookup-key global-map (kbd "C-c <f8>"))
  (kbd "C-c <f9>")   (lookup-key global-map (kbd "C-c <f9>"))
  (kbd "C-c <f10>")  (lookup-key global-map (kbd "C-c <f10>"))
  (kbd "C-c <f11>")  (lookup-key global-map (kbd "C-c <f11>"))
  (kbd "C-c <f12>")  (lookup-key global-map (kbd "C-c <f12>"))
  ))

(apply-keys-to-map
 ctrl-z-map
 (list
  (kbd "1")       (lookup-key global-map [f1])
  (kbd "2")       (lookup-key global-map [f2])
  (kbd "3")       (lookup-key global-map [f3])
  (kbd "4")       (lookup-key global-map [f4])
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

  (kbd "C-x 1")   (lookup-key global-map (kbd "C-x <f1>"))
  (kbd "C-x 2")   (lookup-key global-map (kbd "C-x <f2>"))
  (kbd "C-x 3")   (lookup-key global-map (kbd "C-x <f3>"))
  (kbd "C-x 4")   (lookup-key global-map (kbd "C-x <f4>"))
  (kbd "C-x 5")   (lookup-key global-map (kbd "C-x <f5>"))
  (kbd "C-x 6")   (lookup-key global-map (kbd "C-x <f6>"))
  (kbd "C-x 7")   (lookup-key global-map (kbd "C-x <f7>"))
  (kbd "C-x 8")   (lookup-key global-map (kbd "C-x <f8>"))
  (kbd "C-x 9")   (lookup-key global-map (kbd "C-x <f9>"))
  (kbd "C-x 0")   (lookup-key global-map (kbd "C-x <f10>"))
  (kbd "C-x -")   (lookup-key global-map (kbd "C-x <f11>"))
  (kbd "C-x =")   (lookup-key global-map (kbd "C-x <f12>"))

  (kbd "C-c 1")   (lookup-key global-map (kbd "C-c <f1>"))
  (kbd "C-c 2")   (lookup-key global-map (kbd "C-c <f2>"))
  (kbd "C-c 3")   (lookup-key global-map (kbd "C-c <f3>"))
  (kbd "C-c 4")   (lookup-key global-map (kbd "C-c <f4>"))
  (kbd "C-c 5")   (lookup-key global-map (kbd "C-c <f5>"))
  (kbd "C-c 6")   (lookup-key global-map (kbd "C-c <f6>"))
  (kbd "C-c 7")   (lookup-key global-map (kbd "C-c <f7>"))
  (kbd "C-c 8")   (lookup-key global-map (kbd "C-c <f8>"))
  (kbd "C-c 9")   (lookup-key global-map (kbd "C-c <f9>"))
  (kbd "C-c 0")   (lookup-key global-map (kbd "C-c <f10>"))
  (kbd "C-c -")   (lookup-key global-map (kbd "C-c <f11>"))
  (kbd "C-c =")   (lookup-key global-map (kbd "C-c <f12>"))
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

(apply-keys-to-map
 help-map
 (list
  (kbd "1")       (lookup-key f4-map (kbd "1"))
  (kbd "2")       (lookup-key f4-map (kbd "2"))
  (kbd "3")       (lookup-key f4-map (kbd "3"))
  (kbd "4")       (lookup-key f4-map (kbd "4"))
  (kbd "5")       (lookup-key f4-map (kbd "5"))
  (kbd "6")       (lookup-key f4-map (kbd "6"))
  (kbd "7")       (lookup-key f4-map (kbd "7"))
  (kbd "8")       (lookup-key f4-map (kbd "8"))
  (kbd "9")       (lookup-key f4-map (kbd "9"))
  (kbd "0")       (lookup-key f4-map (kbd "0"))
  (kbd "-")       (lookup-key f4-map (kbd "-"))
  (kbd "=")       (lookup-key f4-map (kbd "="))
  (kbd "\\")      (lookup-key f4-map (kbd "\\"))
  (kbd "DEL")     (lookup-key f4-map (kbd "DEL"))
  (kbd "`")       (lookup-key f4-map (kbd "`"))
  ))

(apply-keys-to-map
 mode-specific-map
 (list
  (kbd "1")       (lookup-key f4-map (kbd "1"))
  (kbd "2")       (lookup-key f4-map (kbd "2"))
  (kbd "3")       (lookup-key f4-map (kbd "3"))
  (kbd "4")       (lookup-key f4-map (kbd "4"))
  (kbd "5")       (lookup-key f4-map (kbd "5"))
  (kbd "6")       (lookup-key f4-map (kbd "6"))
  (kbd "7")       (lookup-key f4-map (kbd "7"))
  (kbd "8")       (lookup-key f4-map (kbd "8"))
  (kbd "9")       (lookup-key f4-map (kbd "9"))
  (kbd "0")       (lookup-key f4-map (kbd "0"))
  (kbd "-")       (lookup-key f4-map (kbd "-"))
  (kbd "=")       (lookup-key f4-map (kbd "="))
  (kbd "\\")      (lookup-key f4-map (kbd "\\"))
  (kbd "DEL")     (lookup-key f4-map (kbd "DEL"))
  (kbd "`")       (lookup-key f4-map (kbd "`"))

  (kbd "C-1")     (lookup-key f4-map (kbd "C-1"))
  (kbd "C-2")     (lookup-key f4-map (kbd "C-2"))
  (kbd "C-3")     (lookup-key f4-map (kbd "C-3"))
  (kbd "C-4")     (lookup-key f4-map (kbd "C-4"))
  (kbd "C-5")     (lookup-key f4-map (kbd "C-5"))
  (kbd "C-6")     (lookup-key f4-map (kbd "C-6"))
  (kbd "C-7")     (lookup-key f4-map (kbd "C-7"))
  (kbd "C-8")     (lookup-key f4-map (kbd "C-8"))
  (kbd "C-9")     (lookup-key f4-map (kbd "C-9"))
  (kbd "C-0")     (lookup-key f4-map (kbd "C-0"))
  (kbd "C--")     (lookup-key f4-map (kbd "C--"))
  (kbd "C-=")     (lookup-key f4-map (kbd "C-="))
  (kbd "C-\\")    (lookup-key f4-map (kbd "C-\\"))
  (kbd "C-DEL")   (lookup-key f4-map (kbd "C-DEL"))
  (kbd "C-`")     (lookup-key f4-map (kbd "C-`"))

  (kbd "M-1")     (lookup-key f4-map (kbd "M-1"))
  (kbd "M-2")     (lookup-key f4-map (kbd "M-2"))
  (kbd "M-3")     (lookup-key f4-map (kbd "M-3"))
  (kbd "M-4")     (lookup-key f4-map (kbd "M-4"))
  (kbd "M-5")     (lookup-key f4-map (kbd "M-5"))
  (kbd "M-6")     (lookup-key f4-map (kbd "M-6"))
  (kbd "M-7")     (lookup-key f4-map (kbd "M-7"))
  (kbd "M-8")     (lookup-key f4-map (kbd "M-8"))
  (kbd "M-9")     (lookup-key f4-map (kbd "M-9"))
  (kbd "M-0")     (lookup-key f4-map (kbd "M-0"))
  (kbd "M--")     (lookup-key f4-map (kbd "M--"))
  (kbd "M-=")     (lookup-key f4-map (kbd "M-="))
  (kbd "M-\\")    (lookup-key f4-map (kbd "M-\\"))
  (kbd "M-DEL")   (lookup-key f4-map (kbd "M-DEL"))
  (kbd "M-`")     (lookup-key f4-map (kbd "M-`"))
  ))


(provide 'fkey-setting)

;;; fkey-setting.el ends here
