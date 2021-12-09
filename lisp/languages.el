;;; languages.el --- Configure the languages I work in -*- lexical-binding: t -*-

;;; Code:

;; Clojure
(use-package clojure-mode)

(use-package cider
  :config
  ;; We use clojure-lsp for showing documentation
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-repl-display-help-banner nil))

;; Elixir
(use-package elixir-mode)

;; Languages which don't require any configuration
(use-package fish-mode)
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(provide 'languages)
;;; languages.el ends here
