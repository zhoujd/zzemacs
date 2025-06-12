;;;version control setting
;;

(zz/load-path "elisp")

;;add mode support
(setq auto-mode-alist
   (append
    (list (cons "\\.hgrc$"         'conf-mode))
    (list (cons "\\.gitconfig$"    'conf-mode))
    auto-mode-alist))

;;mercurial support
(require 'mercurial)
(require 'mq)

(zz/load-path "site-lisp/monky")
(require 'monky)
(setq monky-process-type 'cmdserver)

;;magit support
;;https://magit.vc/manual/magit.html
;;M-x magit-log-buffer-file
;;M-x magit-find-file
;;File log: C-x v l
(zz/load-path "site-lisp/magit/lisp")
(require 'magit)
(defkeys-map global-map
  ((kbd "M-g M-s") 'magit-status)
  ((kbd "M-g M-d") 'magit-dispatch)
  ((kbd "M-g M-f") 'magit-file-dispatch))
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
(defkeys-map  magit-section-mode-map
  ((kbd "M-1") nil)
  ((kbd "M-2") nil)
  ((kbd "M-3") nil)
  ((kbd "M-4") nil))

(defun zz/magit-quit ()
  "Quit magit buffers"
  (interactive)
  (magit-mode-bury-buffer t))

(mapc
 (lambda (map)
   (defkeys-map map ((kbd "Q") 'zz/magit-quit)))
 (list
  magit-mode-map
  magit-log-mode-map
  magit-diff-mode-map
  magit-process-mode-map
  ))

(mapc
 (lambda (map)
   (defkeys-map map ((kbd "C-x g") 'ido-enter-magit-status)))
 (list
  ido-common-completion-map))

;;make `magit-blame' echo with sidebar style.
(custom-set-variables
 '(magit-blame-echo-style 'margin))

;;magit-no-confirm
(add-to-list 'magit-no-confirm 'stage-all-changes)

;;magit-lfs
(require 'magit-lfs)

;;magit-imerge
(require 'magit-imerge)

;;diffstat
(require 'diffstat)
(add-hook 'diff-mode-hook (lambda ()
                            (local-set-key "\C-c\C-l" 'diffstat)))

;;github pull request
(require 'github-pullrequest)

;;gitlab ci mode
(require 'gitlab-ci-mode)

;;git-gutter
;;https://github.com/emacsorphanage/git-gutter
;;https://github.com/nonsequitur/git-gutter-plus
(require 'git-gutter+)

;;https://github.com/emacsmirror/git-timemachine
(require 'git-timemachine)
(defkeys-map global-map
  ((kbd "M-g M-t") 'git-timemachine-toggle))

;;git elisp interface
(require 'git)


(provide 'vc-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; vc-setting.el ends here
