;;; pet-notes.el --- note taking with org mode and denote -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)

(defvar pet/notes-directory "~/Notes")

(defun pet/current-project-root ()
  "Return the root directory of the current project."
  (when-let* ((project (project-current)))
    (project-root project)))

(use-feature org
  :preface
  (defun pet/find-project-note ()
    "Find and open the current project note."
    (interactive)
    (let* ((project-root (pet/current-project-root))
           (notes-file (concat project-root "NOTES.org")))
      (find-file notes-file)
      (goto-char (point-min))))

  (defun pet/insert-project-note ()
    "Insert a note for the current project in the NOTES.org file."
    (interactive)
    (let* ((project-root (pet/current-project-root))
           (notes-file (concat project-root "NOTES.org"))
           (today (format-time-string "%Y-%m-%d %a"))
           (today-header (concat "<" today ">")))
      (find-file notes-file)
      (goto-char (point-min))
      (if (search-forward today-header nil t)
          (progn
            (org-end-of-subtree)
            (org-end-of-item))
        (progn
          (goto-char (point-min))
          (search-forward-regexp "^\\*\\* ")
          (beginning-of-line)
          (org-insert-heading)
          (insert today-header)))))

  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  :bind
  (:map project-prefix-map (("N" . pet/insert-project-note)
                            ("n" . pet/find-project-note))))

;; Notes with Denote
(use-package denote
  :custom
  (denote-directory pet/notes-directory)
  (denote-known-keywords '("journal" "projects" "ideas" "people" "posts" "interviews"))
  :bind (("C-c N" . denote-create-note)
         ("C-c n" . denote-open-or-create)
         ("C-c j" . denote-journal-extras-new-or-existing-entry)))

(use-package consult-denote
  :after (consult denote)
  :config
  (consult-denote-mode))

;; Distraction-free writing
(use-package olivetti
  :demand
  :preface
  (defun pet/writing-mode ()
    "Distraction-free writing environment."
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 1)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 1))))
  :init
  (setq olivetti-body-width .6)
  :hook
  (org-mode . visual-line-mode)
  :bind
  (("<f9>" . pet/writing-mode)))

(provide 'pet-notes)
;;; pet-notes.el ends here
