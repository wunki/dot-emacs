;;; languages.el --- setup differeng languages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Configuration for the different languages that I may use.
;;
;;; Code:
;;
(require 'lib)

(use-package eros
  :demand
  :commands eros-mode
  :config (eros-mode t))

;; Automatically format Emacs lisp code
(use-package elisp-autofmt
  :commands (elisp-autofmt-save-hook-for-this-buffer)
  :straight
  (elisp-autofmt
    :type git
    :host gitlab
    :files (:defaults "elisp-autofmt")
    :repo "ideasman42/emacs-elisp-autofmt")
  :hook (emacs-lisp-mode . elisp-autofmt-save-hook-for-this-buffer))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path
    'inherit
    "inherit the load path so it can find all libraries")
  (flycheck-check-syntax-automatically
    '(mode-enabled save)
    "only check on save"))

;; Clojure
(use-package clojure-mode)

(use-package cider
  :config
  ;; We use clojure-lsp for showing documentation
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-repl-display-help-banner nil))

;; Elixir
(use-package elixir-mode)

;; Zig
(use-package zig-mode
  :config (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

(use-package fish-mode)
(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package lfe-mode
  :if (executable-find "lfe"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'languages)
;;; languages.el ends here
