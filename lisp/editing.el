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

;; Magical Git GUI
(use-package magit
  :bind ("C-c g" . magit-status))

;; Show line changes in the gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode 't))

(provide 'editing)
;;; editing.el ends here
