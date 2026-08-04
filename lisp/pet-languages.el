;;; pet-languages.el --- programming language configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'no-littering)
(require 'pet-packages)
(require 'treesit)

;; Built-in modes ship their own grammar sources. Clojure's third-party mode
;; does not, so keep its source explicit.
(setq treesit-language-source-alist
      '((clojure "https://github.com/sogaiu/tree-sitter-clojure")))

(setopt treesit-auto-install-grammar 'always ; install missing grammars on demand
        treesit-enabled-modes t)             ; prefer the *-ts-mode variant everywhere

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
  :custom
  ;; IELM input history is now persisted across sessions.
  (ielm-history-file-name (no-littering-expand-var-file-name "ielm-history.eld"))
  :config
  (setq ielm-prompt "λ "))

;; Dim parentheses in Lisp
(use-package paren-face
  :config
  (global-paren-face-mode 1))

(use-package rainbow-delimiters
  :hook ((lisp-mode
          emacs-lisp-mode
          clojure-mode
          clojure-ts-mode
          cider-repl-mode) . rainbow-delimiters-mode))

;; Lightweight configuration and data formats
(use-feature json-ts-mode
  :mode ("\\.json\\'" "\\.jsonc\\'"))

(use-feature toml-ts-mode
  :mode "\\.toml\\'")

(use-feature lua-ts-mode
  :mode "\\.lua\\'")

;; Clojure
(use-package clojure-mode
  :custom
  (clojure-toplevel-inside-comment-form t)
  (clojure-indent-style 'align-arguments)
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljd\\'" . clojuredart-mode)
         ("\\.edn\\'" . edn-mode))
  :hook ((clojure-mode
          clojurescript-mode
          clojurec-mode
          clojuredart-mode
          edn-mode) . subword-mode)
  :bind (:map clojure-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)))

(use-package cider
  :after clojure-mode
  :hook ((clojure-mode
          clojurescript-mode
          clojurec-mode
          clojuredart-mode) . cider-mode)
  :custom
  (cider-eldoc-display-for-symbol-at-point nil)
  (cider-repl-display-help-banner nil)
  (cider-save-file-on-load t)
  (cider-history-file (no-littering-expand-var-file-name "nrepl-history"))
  (nrepl-hide-special-buffers t)
  (cider-repl-display-output-before-window-boundaries t)
  (cider-use-xref nil)
  (cider-font-lock-dynamically nil)
  :bind (:map cider-repl-mode-map
              ("C-c C-l" . cider-repl-clear-buffer)))

;; Documentation
(use-feature eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package eldoc-box
  :bind ("C-c d" . eldoc-box-help-at-point))

;; Fish shell
(use-package fish-mode
  :mode "\\.fish\\'")

;; YAML (built-in tree-sitter mode)
(use-feature yaml-ts-mode
  :mode "\\.ya?ml\\'")

;; Markdown (built-in tree-sitter mode, still experimental: live-fontified
;; code blocks, inline images, Org-like heading editing)
(use-feature markdown-ts-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))

;; Docker (built-in tree-sitter mode)
(use-feature dockerfile-ts-mode
  :mode "Dockerfile\\'")

;; Highlight TODO/FIXME/etc
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:slant italic))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-ts-mode . hl-todo-mode)))

;; SQL formatting
(use-package sqlformat
  :if (executable-find "sqlfluff")
  :hook (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'sqlfluff
        sqlformat-args '("--dialect" "postgres")))

(provide 'pet-languages)
;;; pet-languages.el ends here
