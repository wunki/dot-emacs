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
  :config
  ;; we can't add this to :hook because we want it to be
  ;; buffer local
  (add-hook 'before-save-hook #'eglot-format-buffer nil t)
  (setq eglot-autoshutdown t
        eglot-autoreconnect t
        eglot-extend-to-xref t
        eglot-events-buffer-size nil
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:hoverProvider))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/.local/share/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs '(c-mode "clangd"))
  :hook ((zig-mode elixir-ts-mode c-mode) . eglot-ensure))

(provide 'language-server)
;;; language-server.el ends here
