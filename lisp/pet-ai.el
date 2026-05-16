;;; pet-ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :commands (gptel gptel-send gptel-menu gptel-rewrite gptel-add)
  :bind (("C-c a" . gptel-add)
         ("C-c i" . gptel-menu))
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

(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :commands (eca eca-stop eca-restart)
  :bind ("C-c e" . eca)
  :custom
  (eca-chat-custom-model "openai/gpt-5.5")
  (eca-chat-use-side-window t)
  (eca-chat-window-side 'right)
  (eca-chat-window-width 90)
  (eca-chat-focus-on-open t))

(provide 'pet-ai)
;;; pet-ai.el ends here
