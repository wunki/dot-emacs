;;; languages-server.el --- Language server are the new way of communicating with the compiler -*- lexical-binding: t -*-

;;; Code:

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'language-server)
;;; language-server.el ends here
