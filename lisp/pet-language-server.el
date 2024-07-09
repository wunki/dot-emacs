;;; pet-language-server.el --- setup LSP server -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Setup the `eglot' package to intrude as little as possible
;; and work with the languages I use.
;;
;;; Code:
;;
(require 'pet-lib)

(use-feature eglot
  :preface
  (defun pet/eglot-organize-imports ()
    "Organizes the imports."
    (interactive)
    (eglot-code-actions nil nil "source.organizeImports" t))

  (defun pet/eglot-format-buffer-on-save ()
    "Format's the buffer on save."
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

  (defun pet/eglot-organize-imports-on-save ()
    "Organizes the imports on save."
    (add-hook 'before-save-hook #'pet/eglot-organize-imports nil t))
  
  :config
  ;; we can't add this to :hook because we want it to be
  ;; buffer local.
  (setq eglot-autoshutdown t
        eglot-autoreconnect t
        eglot-extend-to-xref t
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities
        '(:foldingRangeProvider))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/.local/share/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs '(c-ts-mode "clangd"))
  :hook (((elixir-ts-mode heex-ts-mode c-ts-mode) . eglot-ensure)
         (elixir-ts-mode . pet/eglot-format-buffer-on-save)
         (c-ts-mode . pet/eglot-format-buffer-on-save))
  :bind (:map eglot-mode-map
              ("C-c C-f" . #'eglot-format-buffer)
              ("C-c a r" . #'eglot-rename)
              ("C-c C-c" . #'eglot-code-actions)))

;; Use flycheck instead of flymake in eglot
(use-package flycheck-eglot
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

(provide 'pet-language-server)
;;; pet-language-server.el ends here
