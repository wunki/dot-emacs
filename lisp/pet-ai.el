;;; pet-ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Claude Code CLI integration
(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el"
       :rev :newest)
  :config
  (claude-code-mode)
  :bind-keymap ("C-c C" . claude-code-command-map))

(use-package gptel
  :custom
  (gptel-stream t)
  (gptel-default-mode 'org-mode)
  (gptel-model "claude-sonnet-4-20250514")
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key #'get-anthropic-api-key))
  :hook
  (gptel-post-stream . gptel-auto-scroll)
  (gptel-post-response-functions . gptel-end-of-response)
  :bind
  ("C-c a" . gptel))

(provide 'pet-ai)
;;; pet-ai.el ends here
