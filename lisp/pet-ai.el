;;; pet-ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :commands (gptel gptel-send gptel-menu gptel-rewrite)
  :init
  (setq gptel-model 'gpt-5.5)
  :config
  (setq gptel-backend
        (gptel-make-openai "OpenAI"
          :key 'gptel-api-key
          :stream t)))

(provide 'pet-ai)
;;; pet-ai.el ends here
