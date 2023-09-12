;;; notes.el --- setup my note taking in Emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This uses Denote for my note taking system.
;; 
;;; Code:
;;
(require 'lib)

(defvar pet/notes-directory
  (if (pet/is-mac)
      "~/Notes"
    "~/notes"))

(use-package denote
  :custom
  (denote-directory pet/notes-directory)
  (denote-known-keywords '("journal" "projects" "ideas" "people"))
  :bind (("C-c N" . denote-create-note)
         ("C-c n" . denote-open-or-create)))

(provide 'notes)
;;; notes.el ends here
