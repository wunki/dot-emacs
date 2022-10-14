;;; language-server.el --- setup LSP server -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Setup the `eglot' package to intrude as little as possible
;; and work with the languages I use.
;;
;;; Code:
;;
(require 'lib)

(use-package eglot
  :hook (((zig-mode elixir-mode c-mode) . eglot-ensure)
         (before-save . eglot-format-buffer))
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode "~/.local/share/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs '(c-mode "clangd")))

(provide 'language-server)
;;; language-server.el ends here
