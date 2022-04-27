;;; language-server.el --- setup LSP server -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Setup the `lsp' package to intrude as little as possible
;; and work with the languages I use.
;;
;;; Code:
;;
(require 'lib)

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-to-list 'exec-path "~/.local/share/elixir-ls/release")
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-mode t)
  (lsp-zig-zls-executable "/usr/local/bin/zls")

  ;; Elixir specific
  (lsp-elixir-suggest-specs nil)

  :hook
  ((clojure-mode . lsp)
    (elixir-mode . lsp)
    (zig-mode . lsp)
    (before-save . lsp-format-buffer)
    (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-sync-mode
  :config (lsp-treemacs-sync-mode 1))

(provide 'language-server)
;;; language-server.el ends here
