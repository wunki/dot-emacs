;;; editing.el --- Configure the overall editing experience -*- lexical-binding: t -*-

;;; Code:

;; Easily select larger chunks of text
(use-package expand-region
  :commands er/expand-region
  :bind ("C-c e" . er/expand-region))

;; Semantic parser for languages, which will give us nicer
;; syntax highlighting
(use-package tree-sitter
  :straight (tree-sitter :type git
                         :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el"))
  :config (add-to-list 'tree-sitter-major-mode-language-alist '(rustic-mode . rust))
  :hook ((python-mode rustic-mode) . tree-sitter-hl-mode))

;; Install all languages available
(use-package tree-sitter-langs
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)

(provide 'editing)
;;; editing.el ends here
