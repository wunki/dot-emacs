;;; pet-notes.el --- note taking with org mode and denote -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)
(require 'project)

(defvar pet/notes-directory "~/Notes")

(defun pet/current-project-root ()
  "Return the root directory of the current project."
  (when-let* ((project (project-current)))
    (project-root project)))

(defun pet/project-notes-file ()
  "Return the NOTES.org file for the current project."
  (if-let* ((project-root (pet/current-project-root)))
      (expand-file-name "NOTES.org" project-root)
    (user-error "Not in a project")))

(defun pet/project-note-title (notes-file)
  "Return a title for NOTES-FILE based on its project directory."
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory notes-file))))

(defun pet/ensure-project-note ()
  "Open and initialize the current project's NOTES.org file."
  (let ((notes-file (pet/project-notes-file)))
    (find-file notes-file)
    (when (= (point-min) (point-max))
      (insert "* " (pet/project-note-title notes-file) "\n\n"))
    (goto-char (point-min))))

(use-feature org
  :preface
  (defun pet/find-project-note ()
    "Find and open the current project note."
    (interactive)
    (pet/ensure-project-note))

  (defun pet/insert-project-note ()
    "Insert a note under today's heading in the current project note."
    (interactive)
    (let* ((today-header (format-time-string "<%Y-%m-%d %a>"))
           (today-heading-regexp
            (concat "^\\*\\* " (regexp-quote today-header) "\\s-*$")))
      (pet/ensure-project-note)
      (cond
       ((re-search-forward today-heading-regexp nil t)
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (unless (looking-back "\n\n" nil) (insert "\n")))
       ((re-search-forward "^\\*\\s-+" nil t)
        (forward-line 1)
        (insert "\n** " today-header "\n\n")
        (forward-line -1))
       (t
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n** " today-header "\n\n")
        (forward-line -1)))))

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
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind (("C-c N" . denote)
         ("C-c n" . denote-open-or-create))
  :config
  (denote-rename-buffer-mode 1))

(use-package denote-journal
  :bind (("C-c j" . denote-journal-new-or-existing-entry)))

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
