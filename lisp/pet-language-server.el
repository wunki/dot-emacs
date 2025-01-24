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

;; LSP-mode
(use-package lsp-mode
  :preface
  (defun pet/lsp-mode-setup-completion ()
    "Set up Corfu and orderless for completions."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  
  (defun pet/format-and-clean-imports-save-hooks ()
    "Set up before-save hooks to format and clean imports."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun pet/lsp-mode-setup-clojure ()
    "Set up Clojure, removing Cider for completions, but enabling it for eldoc."
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)
    (setq-local lsp-eldoc-enable-hover nil))
  
  :hook (((clojure-ts-mode clojurescript-mode clojurec-mode zig-mode go-ts-mode elixir-ts-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . pet/lsp-mode-setup-completion)
         (clojure-ts-mode . pet/format-and-clean-imports-save-hooks)
         (go-ts-mode . pet/format-and-clean-imports-save-hooks))
  :init
  ;; Configure LSP itself
  (setq lsp-keymap-prefix "C-c C-l")
  (setq lsp-completion-provider :none) ;; we use Corfu for completions
  (setq lsp-headerline-breadcrumb-enable nil) ;; Don't need file path in my buffer
  (setq lsp-lens-enable nil) ;; Hide clutter (reference and test counts)
  (setq lsp-enable-indentation nil) ;; use indentation from the mode itself.
  (setq lsp-inlay-hint-enable t)
  (setq lsp-semantic-tokens-enable t) ;; show semantic tokens
  ;; Don't clutter the modeline
  (setq lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil)
  (setq lsp-enable-symbol-highlighting nil) ;; Don't highlight current symbol
  ;; Configure Elixir
  (setq lsp-elixir-server-command '("~/.local/share/elixir-ls/release/language_server.sh"))
)

(use-package lsp-grammarly
  :after lsp-mode
  :hook (org-mode . (lambda ()
                      (require 'lsp-grammarly)
                      (lsp-deferred))))

;;
;; Eglot -- disabled for now because of bug with project.
;;

(use-feature eglot
  :disabled
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
  (add-to-list 'eglot-server-programs
               '(astro-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk "./node_modules/typescript/lib")))))
  :hook (((elixir-ts-mode heex-ts-mode c-ts-mode astro-mode) . eglot-ensure)
         (elixir-ts-mode . pet/eglot-format-buffer-on-save)
         (c-ts-mode . pet/eglot-format-buffer-on-save))
  :bind (:map eglot-mode-map
              ("C-c C-f" . #'eglot-format-buffer)
              ("C-c a r" . #'eglot-rename)
              ("C-c C-c" . #'eglot-code-actions)))

;; Use flycheck instead of flymake in eglot
(use-package flycheck-eglot
  :disabled
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

(provide 'pet-language-server)
;;; pet-language-server.el ends here
