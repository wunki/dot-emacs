;;; pet-ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Claude Code CLI integration
(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el"
       :rev :newest)
  :config
  (claude-code-mode)
  :bind-keymap ("C-c C" . claude-code-command-map)
  :bind (:map project-prefix-map ("C" . claude-code)))

(provide 'pet-ai)
;;; pet-ai.el ends here
