;;; languages-server.el --- Language server are the new way of communicating with the compiler -*- lexical-binding: t -*-

;;; Code:

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-to-list 'exec-path "~/.local/share/elixir-ls/release")
  :custom
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-mode t)

  ;; Elixir specific
  (lsp-elixir-suggest-specs nil)

  :hook
  ((clojure-mode . lsp)
   (elixir-mode . lsp)
   (before-save . lsp-format-buffer)
   (before-save . lsp-organize-imports)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(provide 'language-server)
;;; language-server.el ends here
