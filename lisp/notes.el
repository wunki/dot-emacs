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

(use-package org
  :straight (:type built-in)
  :preface
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
  (:map project-prefix-map ("n" . pet/insert-project-note)))

(use-package denote
  :custom
  (denote-directory pet/notes-directory)
  (denote-known-keywords '("journal" "projects" "ideas" "people" "posts" "interviews"))
  :bind (("C-c C-n" . denote-create-note)
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
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :init
  (setq olivetti-body-width .4)
  :hook (org-mode . pet/writing-mode)
  :bind
  (("<f9>" . pet/writing-mode)))

(provide 'notes)
;;; notes.el ends here
