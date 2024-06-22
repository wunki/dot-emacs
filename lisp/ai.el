;;; ai.el --- pairing up with an AI -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is where I augment Emacs with AI.
;; 
;;; Code:
;;

;; An up-to-date required for copilot.
(require 'secrets)

(use-package jsonrpc)

(use-package copilot
  :after jsonrpc
  :ensure (:host github
                 :repo "copilot-emacs/copilot.el"
                 :files ("dist" "*.el"))
  
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

(use-package gptel
 :custom
 (gptel-stream t)
 (gptel-default-mode 'org-mode)
 (gptel-model "claude-3-5-sonnet-20240620")
 :config
 (setq gptel-backend (gptel-make-anthropic "Claude"
                       :stream t
                       :key #'get-anthropic-api-key))
  :bind
  ("C-c a" . gptel))

(provide 'ai)
;;; ai.el ends here
