;;; editing.el --- frictionless text editing -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Useful packages to make the text editing experience as frictionless
;; as possible.
;;
;;; Code:
;;

(require 'lib)

;; Easily select larger chunks of text
(use-package expand-region
  :commands er/expand-region
  :bind ("C-c e" . er/expand-region))

;; Go to the last place I visited the file.
(use-feature saveplace
  :after no-littering
  :custom
  (save-place-file
   (no-littering-expand-var-file-name "save-place.el"))
  :config
  (save-place-mode +1))

;; Auto-saves buffers when I switch between buffers.
(use-package super-save
  :commands super-save-mode
  :config
  (super-save-mode +1))

;; Easily move to the actual beginning of the line, double-tap moves
;; to the first character
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c f" . crux-recentf-find-file)))

;; Move to the last change in the buffer
(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

;; Required for Magit menu's
(use-package transient)

;; Magical Git GUI
(use-package magit
  :after transient
  :preface
  (defun pet/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))

  :custom (git-commit-summary-max-length 50)
  :bind ("C-c g" . magit-status)
  :hook (git-commit-mode . pet/git-commit-auto-fill-everywhere))

(use-package forge
  :after magit
  :bind ("C-c C-g" . forge-dispatch))

;; Create .gitignore files
(use-package gitignore-templates
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

;; Easy copy links to open files
(use-package git-link)

;; Structural editing
(use-package paredit
  :diminish " ()"
  :hook ((clojure-ts-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode . paredit-mode)))

;; Easy snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :bind ((:map yas-keymap
               ("<return>" . yas-exit-all-snippets)
               ("C-e" . yas/goto-end-of-active-field)
               ("C-a" . yas/goto-start-of-active-field)))

  :config
  ;; No dropdowns please, yas
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  ;; Wrap around region
  (setq yas-wrap-around-region t)

  ;; Use yasnippet everywhere
  (yas-global-mode 1))

(provide 'editing)
;;; editing.el ends here
