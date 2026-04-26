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
               '((clojure-mode clojurescript-mode clojurec-mode
                  clojuredart-mode clojure-ts-mode)
                 "clojure-lsp"))
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode) "~/.local/bin/expert_darwin_arm64"))
  (add-to-list 'eglot-server-programs '(c-ts-mode "clangd"))
  (add-to-list 'eglot-server-programs '(odin-ts-mode "ols"))
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode) .
                 ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))

  :hook (((clojure-mode clojurescript-mode clojurec-mode clojuredart-mode
           clojure-ts-mode elixir-ts-mode heex-ts-mode c-ts-mode go-ts-mode
           go-mod-ts-mode rust-ts-mode odin-ts-mode typescript-ts-mode
           tsx-ts-mode js-ts-mode svelte-mode) . eglot-ensure)
         ((clojure-mode clojurescript-mode clojurec-mode clojuredart-mode
           clojure-ts-mode) . pet/eglot-format-buffer-on-save)
         (elixir-ts-mode . pet/eglot-format-buffer-on-save)
         (go-ts-mode . pet/eglot-format-buffer-on-save)
         (go-ts-mode . pet/eglot-organize-imports-on-save)
         (c-ts-mode . pet/eglot-format-buffer-on-save)
         (odin-ts-mode . pet/eglot-format-buffer-on-save)
         (typescript-ts-mode . pet/eglot-format-buffer-on-save)
         (tsx-ts-mode . pet/eglot-format-buffer-on-save)
         (js-ts-mode . pet/eglot-format-buffer-on-save)
         (svelte-mode . pet/eglot-format-buffer-on-save))

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
