;;; pet-language-server.el --- eglot configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Eglot is the built-in LSP client since Emacs 29.
;;
;;; Code:

(require 'pet-lib)

(use-feature eglot
  :preface
  (defun pet/eglot-organize-imports ()
    "Organize imports via code action."
    (interactive)
    (eglot-code-actions nil nil "source.organizeImports" t))

  (defun pet/eglot-format-buffer-on-save ()
    "Format the buffer on save."
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

  (defun pet/eglot-organize-imports-on-save ()
    "Organize imports on save."
    (add-hook 'before-save-hook #'pet/eglot-organize-imports nil t))

  :config
  (setq eglot-autoshutdown t
        eglot-autoreconnect t
        eglot-extend-to-xref t
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:foldingRangeProvider))
  ;; Let orderless work with eglot completions
  (add-to-list 'completion-category-overrides '(eglot (styles orderless basic)))
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojure-ts-mode) "clojure-lsp"))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/.local/share/elixir-ls/language_server.sh"))
  (add-to-list 'eglot-server-programs '(c-ts-mode "clangd"))

  :hook (((clojure-mode clojure-ts-mode elixir-ts-mode heex-ts-mode c-ts-mode go-ts-mode go-mod-ts-mode rust-ts-mode) . eglot-ensure)
         (elixir-ts-mode . pet/eglot-format-buffer-on-save)
         (go-ts-mode . pet/eglot-format-buffer-on-save)
         (go-ts-mode . pet/eglot-organize-imports-on-save)
         (c-ts-mode . pet/eglot-format-buffer-on-save))

  :bind (:map eglot-mode-map
              ("C-c C-f" . eglot-format-buffer)
              ("C-c a r" . eglot-rename)
              ("C-c C-c" . eglot-code-actions)))

;; LSP snippet expansion via tempel
(use-package eglot-tempel
  :after (eglot tempel)
  :config
  (eglot-tempel-mode 1))

(provide 'pet-language-server)
;;; pet-language-server.el ends here
