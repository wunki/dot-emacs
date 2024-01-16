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
  :straight (eglot :type git
                   :host nil
                   :repo "git://git.sv.gnu.org/emacs.git"
                   :files ("lisp/progmodes/eglot.el"))
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
        '(:hoverProvider
          :documentHighlightProvider
          :documentFormattingProvider
          :documentRangeFormattingProvider
          :documentOnTypeFormattingProvider
          :colorProvider
          :inlayHintProvider
          :foldingRangeProvider))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "nextls" "--stdio=true"))
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

;; Use LSP mode for Clojure because it works a bit better than eglot.
(use-package lsp-mode
  :preface
  ;; Set up Corfu and orderless for completions
  (defun pet/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  ;; Set up before-save hooks to format and clean imports.
  (defun pet/format-and-clean-imports-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (defun pet/lsp-mode-setup-clojure ()
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)
    (setq-local lsp-eldoc-enable-hover nil))
  :hook (((clojure-ts-mode clojurescript-mode clojurec-mode zig-mode go-ts-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . pet/lsp-mode-setup-completion)
         (clojure-ts-mode . pet/format-and-clean-imports-save-hooks)
         (go-ts-mode . pet/format-and-clean-imports-save-hooks))
  :init
  (setq lsp-completion-provider :none) ;; we use Corfu for completions
  (setq lsp-headerline-breadcrumb-enable nil) ;; Don't need file path in my buffer
  (setq lsp-lens-enable nil) ;; Hide clutter (reference and test counts)
  (setq lsp-enable-indentation nil) ;; use indentation from the mode itself.
  ;; Don't clutter modeline...
  (setq lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil)
  (setq lsp-enable-symbol-highlighting nil) ;; Don't highlight current symbol
)

(use-package lsp-grammarly
  :hook (org-mode . (lambda ()
                      (require 'lsp-grammarly)
                      (lsp))))

(provide 'language-server)
;;; language-server.el ends here
