;;;; evil-setting.el --- evil setting file
;;;

;;http://emacswiki.org/emacs/Evil
(zz-load-path "site-lisp/evil")
(require 'evil)
(evil-mode t)

;;key setting
(define-key evil-normal-state-map "  " 'ace-jump-mode)
(define-key evil-normal-state-map " k" 'ace-jump-char-mode)
(define-key evil-normal-state-map " l" 'ace-jump-line-mode)
(define-key evil-normal-state-map " s" 'textmate-goto-symbol)
(define-key evil-normal-state-map " m" 'evil-jump-item)
(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map "-" 'delete-other-windows)
(define-key evil-normal-state-map "b" 'ido-switch-buffer)
(define-key evil-normal-state-map "E" 'ido-find-file)
(define-key evil-normal-state-map "\\" 'evil-repeat-find-char-reverse)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
(define-key evil-normal-state-map "L" 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "<C-return>") 'new-line-in-normal-mode)
(define-key evil-normal-state-map (kbd "M-t") 'textmate-goto-file)
(define-key evil-normal-state-map (kbd "M-f") 'dired)
(define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-next)
(define-key evil-normal-state-map (kbd "M-.") 'find-tag)
(define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
(define-key evil-normal-state-map (kbd "C-SPC") 'comment-or-uncomment-region-or-line)
(define-key evil-normal-state-map (kbd "M-k") 'cycle-buffer)
(define-key evil-normal-state-map (kbd "M-K") 'cycle-buffer-backward)

(define-key evil-normal-state-map (kbd "C-k") 'textmate-column-up)
(define-key evil-normal-state-map (kbd "C-j") 'textmate-column-down)
(define-key evil-normal-state-map (kbd "C-l") 'evil-forward-word-begin)
(define-key evil-normal-state-map (kbd "C-h") 'evil-backward-word-begin)

(evil-define-key 'visual global-map (kbd ",re") 'dr/extract-variable)
(evil-define-key 'normal global-map (kbd ",ri") 'dr/inline-variable)

(evil-ex-define-cmd "!" 'shell-command)
(evil-ex-define-cmd "log" 'magit-log)
(evil-ex-define-cmd "[br]branch" 'magit-branch-manager)
(evil-ex-define-cmd "htmlize" 'htmlize-region)
(evil-ex-define-cmd "[mm]minimap" 'toggle-minimap)
(evil-ex-define-cmd "reset-directory" 'reset-current-dir)
(evil-ex-define-cmd "history" 'magit-file-log)
(evil-ex-define-cmd "channel" 'ido-erc-buffer)
(evil-ex-define-cmd "semicolons" 'semi-colonize)
(evil-ex-define-cmd "create-spec" 'rtt/create-spec)
(evil-ex-define-cmd "create-migration" 'railway-create-migration)
(evil-ex-define-cmd "align" 'align-regexp)
(evil-ex-define-cmd "[er]eval-region" 'eval-region)
(evil-ex-define-cmd "[eb]eval-buffer" 'eval-buffer)
(evil-ex-define-cmd "ack" 'ack)
(evil-ex-define-cmd "[al]ack-location" 'ack-location)
(evil-ex-define-cmd "[rc]run-clojure" 'clojure-jack-in)
(evil-ex-define-cmd "[rr]run-ruby" 'run-ruby)
(evil-ex-define-cmd "[rj]run-js" 'run-js)
(evil-ex-define-cmd "[re]run-elisp" 'ielm)
(evil-ex-define-cmd "[rh]run-haskell" 'run-haskell)
(evil-ex-define-cmd "[gl]gist-list" 'gist-list)
(evil-ex-define-cmd "[gr]gist-region" 'gist-region)
(evil-ex-define-cmd "[grp]gist-region-private" 'gist-region-private)
(evil-ex-define-cmd "rserver" 'serve-rails:start-project-server)
(evil-ex-define-cmd "jasmine" 'serve-rails:start-jasmine)
(evil-ex-define-cmd "guard" 'serve-rails:start-guard)
(evil-ex-define-cmd "spork" 'serve-rails:start-spork)
(evil-ex-define-cmd "erc" 'start-erc)
(evil-ex-define-cmd "weather" 'weather)
(evil-ex-define-cmd "rename-in-project" 'dr/rename-in-project)
(evil-ex-define-cmd "[sh]shell" 'shell)
(evil-ex-define-cmd "[de]debug-elisp" 'edebug-defun)
(evil-ex-define-cmd "dired" 'dired)
(evil-ex-define-cmd "twit" 'twit)
(evil-ex-define-cmd "create-spec" 'rtt/create-spec)
(evil-ex-define-cmd "[fb]find-blueprint" 'railgun-find-blueprint)
(evil-ex-define-cmd "[ff]find-factory" 'railgun-find-factory)
(evil-ex-define-cmd "[fs]find-schema" 'railgun-find-schema)
(evil-ex-define-cmd "[19]onenineify" 'ruby-onenine-ify-region-hashes)

;;esc quit
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)

;;org
(evil-define-key 'normal org-mode-map
  (kbd "<tab>") 'org-cycle
  (kbd "M-L") 'org-metaright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "C-=") 'org-todo
  (kbd "M--") 'evil-org-normal-heading
  (kbd "C-j") 'org-forward-same-level
  (kbd "C-k") 'org-backward-same-level
  (kbd "M-_") 'evil-org-heading-after-current)

(evil-define-key 'insert org-mode-map
  (kbd "C-=") 'org-todo
  (kbd "M-l") 'org-metaright
  (kbd "M-h") 'org-metaleft
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M--") 'evil-org-normal-heading
  (kbd "M-_") 'evil-org-heading-after-current)


(provide 'evil-setting)

;;; evil-setting.el ends here
