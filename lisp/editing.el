;;; editing.el --- Configure the overall editing experience -*- lexical-binding: t -*-

;;; Code:

;; Easily select larger chunks of text
(use-package expand-region
  :commands er/expand-region
  :bind ("C-c e" . er/expand-region))

;; Easily move to the actual beginning of the line, double-tap moves
;; to the first character
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

;; Move to the last change in the buffer
(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

;; Semantic parser for languages, which will give us nicer
;; syntax highlighting
(use-package tree-sitter
  :if (executable-find "tree-sitter")
  :straight (tree-sitter :type git
                         :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el" "src" "Cargo.toml" "Cargo.lock"))
  :hook (((rustic-mode
           python-mode
           css-mode
           elixir-mode) . tree-sitter-mode)
         ((rustic-mode
           python-mode
           css-mode
           elixir-mode) . tree-sitter-hl-mode))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(rustic-mode . rust))
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(elixir-mode . elixir)))

(use-package tree-sitter-langs
  :if (executable-find "tree-sitter")
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)

;; Magical Git GUI
(use-package magit
  :preface
  (defun my/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))
  :bind ("C-c g" . magit-status)
  :hook (git-commit-mode-hook . my/git-commit-auto-fill-everywhere)
  :custom
  (git-commit-summary-max-length 50))

;; Show line changes in the gutter
(use-package git-gutter
  :delight
  :config
  (global-git-gutter-mode 't))

;; Setup auto-completion with company
(use-package company
  :config
  (setq company-idle-delay nil) ; we only start auto-complete on tab
  (global-company-mode)
  :bind ("M-TAB" . company-complete)
        ("TAB" . company-indent-or-complete-common))

;; Automatic parens matching
(electric-pair-mode 1)

(provide 'editing)
;;; editing.el ends here
