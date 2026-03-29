;;; pet-ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Claude Code CLI integration
(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el"
       :rev :newest)
  :custom
  (claude-code-toggle-auto-select t)
  :config
  (claude-code-mode)
  :bind-keymap ("C-c C" . claude-code-command-map)
  :bind (:map project-prefix-map ("C" . claude-code))
  :init
  ;; auto-switch to Claude buffer on start (default only does this with C-u)
  (advice-add 'claude-code :around
              (lambda (orig-fn &optional arg)
                (funcall orig-fn (or arg '(4))))))

(provide 'pet-ai)
;;; pet-ai.el ends here
