;;;; ac-setting.el --- auto complete file

;; auto complete
(zz-load-path "site-lisp/auto-complete")
(zz-load-path "site-lisp/popup")

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat zzemacs-path "/site-lisp/auto-complete/dict"))
(ac-config-default)

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline  'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

;;C auto-complete-clang
;;sudo apt install clang
(defun zz:get-include-dirs ()
  (when (executable-find "g++")
    (let* ((command-result (shell-command-to-string "echo \"\" | g++ -v -x c++ -E -"))
           (start-string "#include <...> search starts here:\n")
           (end-string "End of search list.\n")
           (start-pos (string-match start-string command-result))
           (end-pos (string-match end-string command-result))
           (include-string (substring command-result (+ start-pos (length start-string)) end-pos)))
      (split-string include-string))))

;;sudo apt install llvm-dev libclang-dev
;;sudo yum install llvm-devel clang-devel
;;sudo pacman -S clang llvm
;;(executable-find "clang++")
;;https://www.hiroom2.com/2016/10/31/emacs-auto-complete-clang-async-package
(defvar zz:clang-async-p  t "flag for use clang async")
(defun zz:ac-clang-init ()
  (if (executable-find "clang++")
      (progn
        (require 'auto-complete-clang)
        (setq ac-clang-complete-executable "clang++")
        (setq ac-clang-flags
              (mapcar (lambda (item)
                        (concat "-I" item))
                      (zz:get-include-dirs)))
        (add-to-list 'ac-sources 'ac-source-clang))
      (progn
        (message "clang++ is not find"))))


(defun zz:ac-clang-async-init ()
  (if (executable-find "clang-complete")
      (progn
        (require 'auto-complete-clang-async)
        (setq ac-clang-complete-executable "clang-complete")
        (setq ac-clang-flags
              (mapcar (lambda (item)
                        (concat "-I" item))
                      (zz:get-include-dirs)))
        (add-to-list 'ac-sources 'ac-source-clang-async)
        (ac-clang-launch-completion-process))
      (progn
        (message "clang-complete is not find"))))

(if zz:clang-async-p
    (add-hook 'c-mode-common-hook 'zz:ac-clang-async-init)
    (add-hook 'c-mode-common-hook 'zz:ac-clang-init))

;;auto complete c header
(defun zz:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (mapc
   (lambda (inc)
     (add-to-list 'achead:include-directories inc))
   (zz:get-include-dirs)))

(add-hook 'c++-mode-hook 'zz:ac-c-header-init)
(add-hook 'c-mode-hook 'zz:ac-c-header-init)


;;lisp
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'slime-repl-mode)
     (add-to-list 'ac-modes 'slime-mode)
     ))

;;perl plsense
(require 'plsense)
(plsense-config-default)

;;scheme
;;auto-complete setting
(defvar ac-source-scheme
  '((candidates . (lambda ()
                    (require 'scheme-complete)
                    (all-completions ac-target
                                     (car (scheme-current-env))))))
  "Source for scheme keywords.")

;;Auto-complete-mode config
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (setq ac-sources (append ac-sources '(ac-source-scheme)))))


(provide 'ac-setting)

;;;; ac-setting.el --- end here
