;;;; putty-keys.el
;;putty keys decode
;;http://the.earth.li/~sgtatham/putty/0.62/htmldoc/Chapter4.html#config-keyboard

(zz:load-path "elisp")

(apply-keys-to-map
 input-decode-map
 (list
  "\e[H"       [home]
  "\e[F"       [end]
  "\e[D"       [S-left]
  "\e[C"       [S-right]
  "\e[A"       [S-up]
  "\e[B"       [S-down]
  "\e[C"       [S-right]
  "\e[I"       [prior]
  "\e[G"       [next]
  "\e[M"       [f1]
  "\e[Y"       [S-f1]
  "\e[k"       [C-f1]
  "\e\e[M"     [M-f1]
  "\e[N"       [f2]
  "\e[Z"       [S-f2]
  "\e[l"       [C-f2]
  "\e\e[N"     [M-f2]
  "\e[O"       [f3]
  "\e[a"       [S-f3]
  "\e[m"       [C-f3]
  "\e\e[O"     [M-f3]
  "\e[P"       [f4]
  "\e[b"       [S-f4]
  "\e[n"       [C-f4]
  "\e\e[P"     [M-f4]
  "\e[Q"       [f5]
  "\e[c"       [S-f5]
  "\e[o"       [C-f5]
  "\e\e[Q"     [M-f5]
  "\e[R"       [f6]
  "\e[d"       [S-f6]
  "\e[p"       [C-f6]
  "\e\e[R"     [M-f6]
  "\e[S"       [f7]
  "\e[e"       [S-f7]
  "\e[q"       [C-f7]
  "\e\e[S"     [M-f7]
  "\e[T"       [f8]
  "\e[f"       [S-f8]
  "\e[r"       [C-f8]
  "\e\e[T"     [M-f8]
  "\e[U"       [f9]
  "\e[g"       [S-f9]
  "\e[s"       [C-f9]
  "\e\e[U"     [M-f9]
  "\e[V"       [f10]
  "\e[h"       [S-f10]
  "\e[_"       [C-f10]
  "\e\e[V"     [M-f10]
  "\e[W"       [f11]
  "\e[i"       [S-f11]
  "\e[u"       [C-f11]
  "\e\e[W"     [M-f11]
  "\e[X"       [f12]
  "\e[j"       [S-f12]
  "\e[v"       [C-f12]
  "\e\e[X"     [M-f12]
  ))


(provide 'putty-keys)

;;; end of putty-keys.el
