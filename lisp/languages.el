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

;; Formatter for elisp code.
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :custom
  (elisp-autofmt-python-bin "python3"))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path
   'inherit
   "inherit the load path so it can find all libraries")
  (flycheck-check-syntax-automatically
   '(mode-enabled save)
   "only check on save"))

;; Tree-sitter
(use-package tree-sitter
  :hook ((zig-mode
          go-mode
          elixir-mode
          css-mode
          html-mode
          markdown-mode
          c-mode) . tree-sitter-mode)
  :config
  (setq major-mode-remap-alist
        '((go-mode . go-ts-mode))))

(use-package tree-sitter-langs
  :after tree-sitter)

;; Clojure

;; (use-package clojure-ts-mode
;;   :straight (clojure-ts-mode :type git
;;                              :host github
;;                              :repo "clojure-emacs/clojure-ts-mode"))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :hook ((clojure-mode . subword-mode))
  :config
  (setq clojure-indent-style 'align-arguments))

(use-package
 rainbow-delimiters
 :commands rainbow-delimiters-mode
 :hook
 ((emacs-lisp-mode
   lisp-interaction-mode
   ielm-mode
   lisp-mode
   eval-expression-minibuffer-setup
   slime-repl-mode
   clojure-mode
   racket-mode)
  . rainbow-delimiters-mode))

(use-package clj-refactor
  :after clojure-mode
  :commands cljr-add-keybindings-with-prefix
  :hook clojure-mode
  :custom
  (cljr-assume-language-context "clj")
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r"))

(use-package cider
  ;; clean up the buffer before saving
  :functions cider-format-buffer
  :config
  (add-hook 'before-save-hook #'cider-format-buffer nil t)
  :custom
  ;; We use clojure-lsp for showing documentation
  (cider-eldoc-display-for-symbol-at-point nil)

  ;; we don't need the help banner anymore
  (cider-repl-display-help-banner nil)

  ;; save files when evaluating them
  (cider-save-file-on-load t)

  ;; specify the history file
  (cider-history-file "~/.config/emacs/nrepl-history")

  ;; auto-select the error buffer when it's displayed
  (cider-auto-select-error-buffer t)

  ;; pretty print results in repl
  (cider-repl-use-pretty-printing t)

  ;; hide nrepl buffers when switching between buffers
  (nrepl-hide-special-buffers t)

  ;; don't prompt for symbols, try to use the one currently at prompt
  (cider-prompt-for-symbol nil)

  :bind (:map cider-mode-map
              ("C-c C-l" . cider-find-and-clear-repl-output)
              :map cider-repl-mode-map
              ("C-c C-l" . cider-repl-clear-buffer)))

;; Elixir
(use-package elixir-ts-mode)

;; Documentation
(use-package eldoc
  :straight nil
  :delight
  :hook (prog-mode-hook . eldoc-mode))

(use-package eldoc-box
  :hook (eldoc-mode-hook . eldoc-box-hover-mode)
  :commands eldoc-box--default-upper-corner-position-function
  :init
  (setq eldoc-box-position-function #'eldoc-box--default-upper-corner-position-function
        eldoc-box-clear-with-C-g t))

;; Rust
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :preface
  (defun pet/rustic-mode-hook ()
    (when buffer-file-name
      (setq-local buffer-save-without-query t)))
  :bind
  (:map
    rustic-mode-map
    ("M-?" . lsp-find-references)
    ("C-c C-c l" . flycheck-list-errors))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t)

  :config
  ;; if we don't delete rust-mode, after saving, the buffer will
  ;; jump to rust-mode instead of rustic-mode. Which we do not want.
  (setq auto-mode-alist (rassq-delete-all 'rust-mode auto-mode-alist))

  ;; change emacs PATH to include cargo/bin
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setq exec-path (append exec-path '("~/.cargo/bin")))
  (add-hook 'rustic-mode-hook 'pet/rustic-mode-hook))

;; Zig
(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

;; Fish, my shell of choice
(use-package fish-mode)

;; Yaml, yaml
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; TODO: switch to gfm-mode and make it work with tree-sitter.
(use-package
  markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Common Lisp
(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

;; Racket
(use-package racket-mode)

;; Go
(use-package go-mode
  :mode "\\.go\\'")

;; Docker
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; Highlight TODO keywords in source code
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(use-package sqlformat
  :hook (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'sqlfluff)
  (setq sqlformat-args '("--dialect" "postgres")))

(provide 'languages)
;;; languages.el ends here
