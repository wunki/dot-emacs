;;; pet-languages.el --- setup programming languages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Configuration for the different languages that I may use.
;;
;;; Code:
;;
(require 'pet-lib)

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
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-info)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit "inherit the load path so it can find all libraries")
  (flycheck-check-syntax-automatically '(mode-enabled save) "only check on save"))

(use-feature ielm
  :config
  (setq ielm-prompt "λ "))

;; Colorful parenthesis
(use-package rainbow-delimiters
  :disabled
  :commands rainbow-delimiters-mode
  :hook
  ((emacs-lisp-mode
    lisp-interaction-mode
    ielm-mode
    lisp-mode
    eval-expression-minibuffer-setup
    slime-repl-mode
    clojure-ts-mode
    racket-mode)
   . rainbow-delimiters-mode))

(use-package paren-face
  :config
  (global-paren-face-mode))

;; The web
(use-package web-mode
  :mode (".html?$"))

;; Create a new astro mode
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

;; Clojure
(use-package clojure-ts-mode
  :ensure (clojure-ts-mode
           :type git
           :host github
           :repo "clojure-emacs/clojure-ts-mode")
  :custom
  (clojure-toplevel-inside-comment-form t)
  (clojure-ident-style 'align-arguments)
  :hook ((clojure-ts-mode . subword-mode))
  :bind (:map clojure-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)))

(use-package cider
  ;; clean up the buffer before saving
  :after clojure-mode
  :functions cider-format-buffer
  :init
  (add-to-list 'safe-local-variable-values '(cider-clojure-cli-aliases . ":dev"))
  :hook (clojure-ts-mode . cider-mode)
  :custom
  ;; We use clojure-lsp for showing documentation
  (cider-eldoc-display-for-symbol-at-point nil)

  ;; we don't need the help banner anymore
  (cider-repl-display-help-banner nil)

  ;; save files when evaluating them
  (cider-save-file-on-load t)

  ;; specify the history file
  (cider-history-file "~/.config/emacs/var/nrepl-history")

  ;; auto-select the error buffer when it's displayed
  (cider-auto-select-error-buffer t)

  ;; pretty print results in repl
  (cider-repl-use-pretty-printing t)

  ;; hide nrepl buffers when switching between buffers
  (nrepl-hide-special-buffers t)

  ;; always scroll output from interactive evaluations into view
  (cider-repl-display-output-before-window-boundaries t)

  ;; don't prompt for symbols, try to use the one currently at prompt
  (cider-prompt-for-symbol nil)

  ;; use lsp
  (cider-use-xref nil)

  ;; use lsp for semantic tokens
  (cider-font-lock-dynamically nil)

  ;; use lsp
  (cider-prompt-for-symbol nil)

  ;; clean the output
  (nrepl-log-messages nil)

  ;; clear the buffer
  :bind (:map cider-repl-mode-map
              ("C-c C-l" . cider-repl-clear-buffer)))

;; Elixir
(use-package elixir-ts-mode)

;; Documentation
(use-feature eldoc
  :diminish eldoc-mode
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
  :mode ("\\.zig\\'" . zig-mode)
  :hook (zig-mode . auto-fill-mode))

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
  (inferior-lisp-program "sbcl")
  (sly-symbol-completion-mode nil)
  :hook (sly-mrepl-mode . electric-pair-mode))

(use-package sly-asdf
  :after sly
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

;; Racket
(use-package racket-mode)

;; Scheme
(use-package geiser-guile)

;; Go
(use-feature go-ts-mode
  :mode
  ("\\.go\\'" . go-ts-mode)
  ("/go\\.mod\\'" . go-mod-ts-mode)
  :custom (go-ts-mode-indent-offset 4))

(defun pet/project-find-go-module (dir)
  "Find the root of the project by finding go.mod file in DIR."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "I have no idea what this does with PROJECT."
  (cdr project))

(add-hook 'project-find-functions #'pet/project-find-go-module)

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

(provide 'pet-languages)
;;; pet-languages.el ends here
