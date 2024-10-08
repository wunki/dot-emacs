;;; pet-editing.el --- frictionless text editing -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Useful packages to make the text editing experience as frictionless
;; as possible.
;;
;;; Code:
;;

(require 'pet-lib)

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

(provide 'pet-editing)
;;; pet-editing.el ends here
