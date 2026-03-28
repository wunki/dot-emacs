;;; pet-languages.el --- programming language configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)

;; Tree-sitter grammar sources
(setq treesit-language-source-alist
      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")))

;; Auto-install missing tree-sitter grammars
(defun pet/ensure-treesit-grammar (lang)
  "Install tree-sitter grammar for LANG if missing and source is known."
  (when (and (not (treesit-language-available-p lang))
             (assq lang treesit-language-source-alist))
    (message "Installing tree-sitter grammar for %s..." lang)
    (treesit-install-language-grammar lang)
    (message "Installed tree-sitter grammar for %s" lang)))

(add-hook 'after-init-hook
          (lambda ()
            (mapc #'pet/ensure-treesit-grammar
                  (mapcar #'car treesit-language-source-alist))))

;; Inline Emacs Lisp evaluation
(use-package eros
  :hook (emacs-lisp-mode . eros-mode))

;; Elisp formatter
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :custom
  (elisp-autofmt-python-bin "python3"))

;; Flymake (built-in, replaces flycheck)
(use-feature flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

(use-feature ielm
  :config
  (setq ielm-prompt "λ "))

;; Dim parentheses in Lisp
(use-package paren-face
  :config
  (global-paren-face-mode))

;; Web
(use-package web-mode
  :mode ("\\.html?$" "\\.astro$"))

;; Clojure
(use-package clojure-ts-mode
  :custom
  (clojure-ts-auto-remap t)
  (clojure-toplevel-inside-comment-form t)
  (clojure-indent-style 'align-arguments)
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
         ("\\.cljc\\'" . clojure-ts-clojurec-mode)
         ("\\.cljd\\'" . clojure-ts-clojuredart-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook (clojure-ts-mode . subword-mode)
  :bind (:map clojure-ts-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)))

(use-package cider
  :after clojure-ts-mode
  :hook (clojure-ts-mode . cider-mode)
  :custom
  (cider-eldoc-display-for-symbol-at-point nil)
  (cider-repl-display-help-banner nil)
  (cider-save-file-on-load t)
  (cider-history-file (no-littering-expand-var-file-name "nrepl-history"))
  (cider-auto-select-error-buffer t)
  (cider-repl-use-pretty-printing t)
  (nrepl-hide-special-buffers t)
  (cider-repl-display-output-before-window-boundaries t)
  (cider-prompt-for-symbol nil)
  (cider-use-xref nil)
  (cider-font-lock-dynamically nil)
  (nrepl-log-messages nil)
  :bind (:map cider-repl-mode-map
              ("C-c C-l" . cider-repl-clear-buffer)))

;; Elixir
(use-package elixir-ts-mode)

;; Documentation
(use-feature eldoc
  :hook (prog-mode . eldoc-mode))

(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-position-function #'eldoc-box--default-upper-corner-position-function)
  (eldoc-box-clear-with-C-g t))

;; Rust (built-in tree-sitter mode)
(use-feature rust-ts-mode
  :mode "\\.rs\\'")

;; Go (built-in tree-sitter mode)
(use-feature go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :custom (go-ts-mode-indent-offset 4))

(defun pet/project-find-go-module (dir)
  "Find Go module root for DIR."
  (when-let* ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "Return root of Go module PROJECT."
  (cdr project))

(add-hook 'project-find-functions #'pet/project-find-go-module)

;; Zig
(use-package zig-mode
  :mode "\\.zig\\'")

;; Fish shell
(use-package fish-mode)

;; YAML (built-in tree-sitter mode)
(use-feature yaml-ts-mode
  :mode "\\.ya?ml\\'")

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom (markdown-command "multimarkdown"))

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

;; Docker (built-in tree-sitter mode)
(use-feature dockerfile-ts-mode
  :mode "Dockerfile\\'")

;; Highlight TODO/FIXME/etc
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-ts-mode . hl-todo-mode)))

;; SQL formatting
(use-package sqlformat
  :hook (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'sqlfluff
        sqlformat-args '("--dialect" "postgres")))

(provide 'pet-languages)
;;; pet-languages.el ends here
