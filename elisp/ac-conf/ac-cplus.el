;;; Auto Complete for C and C++

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

