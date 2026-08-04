;;; pet-notes.el --- note taking with org mode and denote -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Personal knowledge and journal notes use Denote under `pet/notes-directory'.
;; Project development logs remain regular NOTES.org files in their projects.
;;
;;; Code:

(require 'no-littering)
(require 'pet-packages)
(require 'project)

(defvar pet/notes-directory "~/Notes")

(defvar-keymap pet/notes-map
  :doc "Keymap for creating, connecting, and retrieving notes."
  "n" #'denote
  "f" #'denote-open-or-create
  "l" #'denote-link-or-create
  "b" #'denote-backlinks
  "g" #'denote-grep
  "d" #'denote-dired
  "j" #'denote-journal-new-or-existing-entry
  "r" #'denote-rename-file
  "x" #'denote-region)

(keymap-global-set "C-c n" pet/notes-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c n" "notes"))

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
  :functions (org-end-of-subtree)
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
  ;; Build keyword completion from the notes we actually create.
  (denote-known-keywords nil)
  :hook
  (dired-mode . denote-dired-mode)
  :config
  (denote-rename-buffer-mode 1))

(use-package denote-journal
  :custom
  (denote-journal-title-format 'day-date-month-year))

(use-package consult-denote
  :after (consult denote)
  :config
  (consult-denote-mode 1))

;; Distraction-free writing
(use-package olivetti
  :custom
  (olivetti-body-width 0.6)
  :hook (org-mode . visual-line-mode)
  :bind ("<f9>" . olivetti-mode))

(provide 'pet-notes)
;;; pet-notes.el ends here
