;;; iswitchb-filter.el --- add filter to iswitchb mode
;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'iswitchb-filter)

;;; Code:

(require 'iswitchb)

(defun isb-filter-cond (name type)
  (let* ((isb-filter-dired (with-current-buffer
                            name (derived-mode-p 'dired-mode)))
         (isb-filter-emacs (or (string-match "^\\*.*\\*$" name)
                               (string-match "^ " name)))
         (isb-filter-common (and (not isb-filter-dired)
                                 (not isb-filter-emacs)))
         (isb-filter-select nil))
    (case type
      ((isb-dired)  (setq isb-filter-select isb-filter-dired))
      ((isb-emacs)  (setq isb-filter-select isb-filter-emacs))
      ((isb-common) (setq isb-filter-select isb-filter-common)))
    isb-filter-select))

(defun isb-filter-buffer (type)
  (let ((isb-filter-list '()))
    (dolist (name (iswitchb-make-buflist nil))
            (when (isb-filter-cond name type)
              (setq isb-filter-list (cons name isb-filter-list))))
    (if isb-filter-list
        (setq isb-filter-list (reverse isb-filter-list))
        (setq isb-filter-list iswitchb-matches))
    isb-filter-list))

(defun isb-show-dired ()
   (interactive)
   (setq iswitchb-buflist (isb-filter-buffer 'isb-dired))
   (setq iswitchb-rescan t)
   (delete-minibuffer-contents))

(defun isb-show-common ()
   (interactive)
   (setq iswitchb-buflist (isb-filter-buffer 'isb-common))
   (setq iswitchb-rescan t)
   (delete-minibuffer-contents))

(defun isb-show-emacs ()
   (interactive)
   (setq iswitchb-buflist (isb-filter-buffer 'isb-emacs))
   (setq iswitchb-rescan t)
   (delete-minibuffer-contents))

;;filter group switch
(setq isb-filter-group '(isb-show-common isb-show-dired isb-show-emacs))
(setq isb-filter-index 0)

(defun isb-filter-prev ()
  (interactive)
  (if (= isb-filter-index 0)
      (setq isb-filter-index (- (length isb-filter-group) 1))
      (setq isb-filter-index (- isb-filter-index 1)))
  (apply (nth isb-filter-index isb-filter-group) '()))

(defun isb-filter-next ()
  (interactive)
  (if (= isb-filter-index (- (length isb-filter-group) 1))
      (setq isb-filter-index 0)
      (setq isb-filter-index (+ isb-filter-index 1)))
  (apply (nth isb-filter-index isb-filter-group) '()))

;;reset buffer default
(defun isb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))


(provide 'iswitchb-filter)

;;; iswitchb-filter.el ends here
