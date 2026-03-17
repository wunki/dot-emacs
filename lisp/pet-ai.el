;;; pet-ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
       :rev :newest)
  :preface
  (defun pet/copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command)))
  :bind (:map copilot-mode-map
              ("<tab>" . pet/copilot-tab)
              ("s-n" . copilot-next-completion)
              ("s-p" . copilot-previous-completion)
              ("s-w" . copilot-accept-completion-by-word)
              ("s-l" . copilot-accept-completion-by-line)))

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
