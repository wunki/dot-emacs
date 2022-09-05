;;; languages.el --- setup differeng languages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Configuration for the different languages that I may use.
;;
;;; Code:
;;
(require 'lib)

;; Display Emacs lisp results inline.
(use-package eros
  :demand
  :commands eros-mode
  :config (eros-mode t))

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

;; Documentation
(use-package eldoc
  :straight nil
  :delight
  :hook (prog-mode-hook . eldoc-mode)
  :init
  (global-eldoc-mode 1))

(use-package eldoc-box
  :hook (eldoc-mode-hook . eldoc-box-hover-mode)
  :init
  (setq eldoc-box-position-function #'eldoc-box--default-upper-corner-position-function
        eldoc-box-clear-with-C-g t))

;; Rust
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :bind
  (:map
    rustic-mode-map
    ("M-j" . lsp-ui-imenu)
    ("M-?" . lsp-find-references)
    ("C-c C-c l" . flycheck-list-errors)
    ("C-c C-c a" . lsp-execute-code-action)
    ("C-c C-c r" . lsp-rename)
    ("C-c C-c q" . lsp-workspace-restart)
    ("C-c C-c Q" . lsp-workspace-shutdown)
    ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t)
  (rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))

  :config
  ;; if we don't delete rust-mode, after saving, the buffer will
  ;; jump to rust-mode instead of rustic-mode. Which we do not want.
  (setq auto-mode-alist (rassq-delete-all 'rust-mode auto-mode-alist))

  ;; change emacs PATH to include cargo/bin
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setq exec-path (append exec-path '("~/.cargo/bin")))
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; Zig
(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

;; Fish, my shell of choice
(use-package fish-mode)

;; Yaml, yaml
(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Lisp on the Erlang beam
(use-package lfe-mode
  :if (executable-find "lfe"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Common Lisp
(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

;; Go
(use-package go-mode)

;; Docker
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; Highlight TODO keywords in source code
(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(provide 'languages)
;;; languages.el ends here
