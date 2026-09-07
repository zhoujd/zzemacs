;;;; ai-setting.el --- AI Configure

;; gptel
(zz/load-path "site-lisp/gptel")

(require 'gptel)
(require 'gptel-openai)
(require 'gptel-openai-extras)

;; Log level
;(setq gptel-log-level 'debug)

;; Performance parameters
(setq-default gptel-max-tokens 8192)
(setq-default gptel-directives
              '((default . "\
You are a pragmatic, expert software engineer. \
Provide concise code reviews and direct code improvements with minimal conversational fluff. \
Prefer modern, safe syntax.")))

(setq gptel-track-response t)
(setq gptel-default-mode 'markdown-mode)
(setq gptel-track-response t)
(setq gptel-backend
      (gptel-make-openai "LiteLLM-GLM"
        :host "10.11.70.11:8090"               ; Matches the local IP and port in your config.toml
        :protocol "http"                       ; Explicitly specifies the intranet HTTP protocol
        :endpoint "/v1/chat/completions"       ; Standard LiteLLM / OpenAI routing endpoint
        :stream t                              ; Enables smooth typewriter-style streaming responses
        :models '("GLM-5.2-W4AFP8")            ; Registers your specific model ID
        :key (lambda ()
               (or (getenv "OPENAI_API_KEY")
                   "gpustack_0098b68022a24157_3ffe92dc07e813389f93affada9b365b"))))
(setq gptel-model 'GLM-5.2-W4AFP8)

(defun zz/gptel-menu ()
  "A pragmatic fallback menu for gptel when the built-in transient menu is unavailable."
  (interactive)
  (if (fboundp 'gptel-menu)
      (call-interactively 'gptel-menu)
    ;; Fallback interactive menu powered by Emacs built-in completing-read
    (let* ((actions '("Change Personality/Directive" "Adjust Temperature" "Show Current Status"))
           (action (completing-read "GLM-5.2 Actions: " actions nil t)))
      (cond
       ((string= action "Change Personality/Directive")
        (let* ((personalities '(("Pragmatic" . "Pragmatic expert software engineer. Provide concise code reviews with minimal fluff.")
                                ("Security Expert" . "Strict security auditor. Scan for leaks, vulnerabilities, and overflows.")
                                ("Refactor Master" . "Refactor master. Rewrite code for maximum elegance and modern syntax.")))
               (choice (completing-read "Select Persona: " (mapcar #'car personalities) nil t))
               (directive (cdr (assoc choice personalities))))
          (setq gptel-description directive)
          (message "GLM-5.2 Persona set to: %s" choice)))

       ((string= action "Adjust Temperature")
        (let ((temp (read-number "Enter Temperature (0.0 - 1.0, e.g., 0.2 for strict code): " 0.2)))
          (setq gptel-temperature temp)
          (message "GLM-5.2 Temperature set to: %.1f" temp)))

       ((string= action "Show Current Status")
        (message "Backend: LiteLLM-GLM | Model: %s | Temp: %s"
                 gptel-model (or (bound-and-true-p gptel-temperature) "default")))))))

(defkeys-map global-map
  ((kbd "C-c i a") 'gptel)            ; Create or switch to a dedicated AI chat buffer
  ((kbd "C-c i s") 'gptel-send)       ; Send selected code and prompt (fully supports TRAMP)
  ((kbd "C-c i m") 'zz/gptel-menu))   ; Summon the interactive parameters popup menu


(provide 'ai-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; ai-setting.el --- end here
