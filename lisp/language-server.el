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
  :hook ((zig-mode elixir-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode "~/.local/share/elixir-ls/release/language_server.sh")))

(provide 'language-server)
;;; language-server.el ends here
