;;; notes.el --- configure note taking with org mode -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is where we configure org mode.
;; 
;;; Code:
;;
(require 'lib)

(defvar pet/notes-directory
  (if (pet/is-mac) "~/Notes" "~/notes"))

(defun pet/current-project-root ()
  "Return the root directory of the current project."
  (when-let ((project (project-current)))
    (nth 2 project)))

(use-feature org
  :preface

  (defun pet/find-project-note ()
    "Find and open the current project note"
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

(use-package denote
  :custom
  (denote-directory pet/notes-directory)
  (denote-known-keywords '("journal" "projects" "ideas" "people" "posts" "interviews"))
  :bind (("C-c N" . denote-create-note)
         ("C-c n" . denote-open-or-create)))

;; Distraction-free screen for writing
(use-package olivetti
  :demand
  :preface
  (defun pet/writing-mode ()
    "Distraction-free writing environment"
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
  ;; (org-mode . pet/writing-mode)
  :bind
  (("<f9>" . pet/writing-mode)))


;;; BLOGGING
(defvar pet/blog-directory "~/Developer/Petar/blog")
(defvar pet/blog-frontmatter
  "---
title: \"${title}\"
description: \"\"
date: ${date}
template: \"${type}.html\"
draft: false
taxonomies:
  ${taxonomies}
---

")

(defun pet/write-blog ()
  "Create a new blog note or post."
  (interactive)
  (let* ((is-note (y-or-n-p "Do you want to write a note? "))
         (title (read-string "What is the title? "))
         (tags (read-string "Add tags, leave empty for none: "))
         (filename (concat (pet--slugify title) ".md"))
         (dir-type (if is-note "note" "post"))
         (path (expand-file-name
                filename
                (expand-file-name
                 (concat "content/" dir-type "s") pet/blog-directory)))
         (date (format-time-string "%Y-%m-%d"))
         (replacements
          `(("${title}" . ,title)
            ("${date}" . ,date)
            ("${type}" . ,dir-type)
            ("${taxonomies}" . ,(format (if is-note "tags: [%s]" "categories: [%s]") tags))
            ("${tags}" . ,tags))))
    (find-file path)
    (insert pet/blog-frontmatter) ;; insert template for frontmatter
    (goto-char (point-min)) ;; beginning of buffer, so we can replace the title
    (pet--replace-strings replacements)
    (goto-char (point-max))))

(defun pet--slugify (s)
  "Create a slug from string S."
  (replace-regexp-in-string "^-\\|-$" ""
    (replace-regexp-in-string
     "[^a-z0-9]+" "-" (downcase s))))

(defun pet--replace-strings (replacement-list)
  "Replace multiple strings in the current buffer.

REPLACEMENT-LIST is an alist where each element is a cons cell (SEARCH
. REPLACE).  For each pair, all occurrences of SEARCH are replaced with
REPLACE."
  (save-excursion
    (dolist (rep replacement-list)
      (goto-char (point-min))
      (while (search-forward (car rep) nil t)
        (replace-match (cdr rep) t t)))))

(provide 'notes)
;;; notes.el ends here
