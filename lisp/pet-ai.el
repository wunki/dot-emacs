;;; pet-ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :functions (gptel-make-openai-oauth)
  :defines (gptel-backend gptel-model gptel-openai-oauth-login-method)
  :commands (gptel gptel-send gptel-menu gptel-rewrite gptel-add)
  :bind (("C-c a" . gptel-add)
         ("C-c i" . gptel-menu))
  :init
  (setq gptel-model 'gpt-5.6-sol)
  :config
  (setq gptel-backend (gptel-make-openai-oauth "OpenAI-sub"))
  (setq gptel-openai-oauth-login-method 'device)
  ;; disable the ugly background when things are added
  ;; to the context
  (with-eval-after-load 'gptel-context
    (custom-theme-set-faces
     'user
     '(gptel-context-highlight-face
       ((t (:background unspecified :extend nil)))))))

(provide 'pet-ai)
;;; pet-ai.el ends here
