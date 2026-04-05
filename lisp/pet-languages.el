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
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")
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

;; TypeScript / JavaScript (built-in tree-sitter modes)
(use-feature typescript-ts-mode
  :mode "\\.ts\\'")

(use-feature tsx-ts-mode
  :mode "\\.tsx\\'")

(use-feature js-ts-mode
  :mode ("\\.js\\'" "\\.mjs\\'" "\\.cjs\\'"))

;; CSS (built-in tree-sitter mode)
(use-feature css-ts-mode
  :mode "\\.css\\'")

;; JSON (built-in tree-sitter mode)
(use-feature json-ts-mode
  :mode ("\\.json\\'" "\\.jsonc\\'"))

;; Svelte (requires typescript-mode for script lang="ts" highlighting)
(use-package svelte-mode
  :mode "\\.svelte\\'"
  :hook (svelte-mode . (lambda ()
                         (setq-local indent-tabs-mode t)
                         (setq-local svelte-basic-offset tab-width))))

;; typescript-mode is needed by svelte-mode for TypeScript submode highlighting.
;; Standalone .ts files use typescript-ts-mode (tree-sitter) instead.
(use-package typescript-mode
  :defer t)

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
  :bind ("C-c d" . eldoc-box-help-at-point))

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

;; Odin (tree-sitter mode, no MELPA package exists)
(defvar odin-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?`  "\"" table)
    (modify-syntax-entry ?_  "_"  table)
    table))

(defvar odin-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'odin
   :feature 'comment
   '((comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

   :language 'odin
   :feature 'string
   '((string) @font-lock-string-face
     (character) @font-lock-string-face
     (escape_sequence) @font-lock-escape-face)

   :language 'odin
   :feature 'keyword
   '(["if" "else" "when" "for" "do" "switch" "case"
      "return" "defer" "using" "import" "package" "foreign"
      "proc" "struct" "enum" "union" "bit_set" "bit_field"
      "break" "continue" "or_return" "or_break" "or_continue" "or_else"
      "cast" "auto_cast" "transmute" "distinct" "dynamic"
      "map" "matrix" "where" "in" "not_in"] @font-lock-keyword-face)

   :language 'odin
   :feature 'constant
   '((nil) @font-lock-constant-face
     (uninitialized) @font-lock-constant-face
     ["true" "false"] @font-lock-constant-face)

   :language 'odin
   :feature 'number
   '((number) @font-lock-number-face
     (float) @font-lock-number-face)

   :language 'odin
   :feature 'type
   :override t
   '((struct_declaration (identifier) @font-lock-type-face)
     (enum_declaration (identifier) @font-lock-type-face)
     (union_declaration (identifier) @font-lock-type-face)
     (const_type_declaration (identifier) @font-lock-type-face)
     (named_type (identifier) @font-lock-type-face))

   :language 'odin
   :feature 'function
   '((procedure_declaration (identifier) @font-lock-function-name-face)
     (call_expression (identifier) @font-lock-function-call-face))

   :language 'odin
   :feature 'attribute
   '((attribute) @font-lock-preprocessor-face
     (tag) @font-lock-preprocessor-face
     (build_tag) @font-lock-preprocessor-face))
  "Tree-sitter font-lock settings for `odin-ts-mode'.")

(define-derived-mode odin-ts-mode prog-mode "Odin"
  "Major mode for Odin, powered by tree-sitter."
  :syntax-table odin-ts-mode--syntax-table
  (when (treesit-ready-p 'odin)
    (treesit-parser-create 'odin)
    (setq-local treesit-font-lock-settings odin-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword constant type)
                  (number function attribute)
                  ()))
    (setq-local treesit-simple-indent-rules
                `((odin
                   ((parent-is "block") parent-bol 4)
                   ((parent-is "struct_type") parent-bol 4)
                   ((parent-is "enum_type") parent-bol 4)
                   ((parent-is "union_type") parent-bol 4)
                   ((parent-is "parameters") parent-bol 4)
                   ((parent-is "switch_case") parent-bol 4)
                   ((node-is "}") parent-bol 0)
                   ((node-is ")") parent-bol 0)
                   (no-node parent-bol 0))))
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "//+\\s-*")
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-ts-mode))

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
