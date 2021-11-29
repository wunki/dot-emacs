;;; languages.el --- Configure the languages I work in -*- lexical-binding: t -*-

;;; Code:

;; Clojure
(use-package clojure-mode)

(use-package cider
  :config
  ;; We use clojure-lsp for showing documentation
  (setq cider-eldoc-display-for-symbol-at-point nil))

(provide 'languages)
;;; languages.el ends here
