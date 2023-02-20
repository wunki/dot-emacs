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
  :hook (((zig-mode elixir-mode c-mode java-ts-mode) . eglot-ensure)
         (before-save . eglot-format-buffer))
  :config
  (setq eglot-autoshutdown t
        eglot-autoreconnect t
        eglot-extend-to-xref t
        eglot-events-buffer-size nil
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:hoverProvider))
  (add-to-list 'eglot-server-programs '(elixir-mode "~/.local/share/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs '(java-ts-mode "jdtls"))
  (add-to-list 'eglot-server-programs '(c-mode "clangd")))

(provide 'language-server)
;;; language-server.el ends here
