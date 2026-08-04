;;; pet-editing.el --- frictionless text editing -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Commands and packages that improve everyday text editing.
;;
;;; Code:

(require 'pet-packages)

(defun pet/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t))))))

(defun pet/move-beginning-of-line (arg)
  "Move to first non-whitespace, or beginning of line if already there.
ARG is passed to `beginning-of-visual-line'."
  (interactive "^p")
  (let ((position (point)))
    (back-to-indentation)
    (when (= position (point))
      (beginning-of-visual-line arg))))

;; Expand selection
(use-package expand-region
  :bind ("C-c e" . er/expand-region))

;; Remember cursor position
(use-feature saveplace
  :config
  (save-place-mode +1))

;; Auto-save visited files (built-in, replaces super-save)
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 1)

;; Smart beginning of line
(global-set-key (kbd "C-a") 'pet/move-beginning-of-line)

;; Jump to last change
(use-package goto-last-change
  :bind ("C-;" . goto-last-change))

;; Structural editing for Lisp
(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (clojure-ts-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode . paredit-mode)
         (lisp-mode . paredit-mode)))

;; Snippets (tempel: lighter, native capf integration with corfu)
(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :init
  (defun pet/tempel-setup-capf ()
    "Add tempel to completion-at-point-functions."
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((prog-mode . pet/tempel-setup-capf)
         (text-mode . pet/tempel-setup-capf)
         (conf-mode . pet/tempel-setup-capf)))

(use-package tempel-collection
  :after tempel)

(provide 'pet-editing)
;;; pet-editing.el ends here
