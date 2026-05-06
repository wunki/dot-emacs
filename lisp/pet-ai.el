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
          :stream t))
  ;; disable the ugly background when things are added
  ;; to the context
  (with-eval-after-load 'gptel-context
    (custom-theme-set-faces
     'user
     '(gptel-context-highlight-face
       ((t (:background unspecified :extend nil)))))))

(provide 'pet-ai)
;;; pet-ai.el ends here
