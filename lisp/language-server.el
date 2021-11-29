;;; languages-server.el --- Language server are the new way of communicating with the compiler -*- lexical-binding: t -*-

;;; Code:

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-mode t)
  :hook
  ((before-save . lsp-format-buffer)
   (before-save . lsp-organize-imports)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'language-server)
;;; language-server.el ends here
