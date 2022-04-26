;;; org.el --- setup org mode -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Setup org and org-roam for note taking
;; 
;;; Code:
;;
(require 'lib)

(if (pet/is-linux)
  (defvar org-directory (expand-file-name "~/notes"))
  (defvar org-directory (expand-file-name "~/Notes")))

(use-package org
  :custom (org-return-follows-link t))

(use-package org-roam
  :after org
  :commands (org-roam org-roam-db-autosync-enable)
  :defines org-roam-v2-ack
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename org-directory))
  (org-startup-indented t)
  (org-roam-dailies-directory "dailies/")
  (org-roam-dailies-capture-templates
    '
    (
      ("d"
        "default"
        entry
        "* %?"
        :target
        (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :config (org-roam-db-autosync-enable)
  :bind
  (("C-c n f" . org-roam-node-find)
    ("C-c n r" . org-roam-node-random)
    ("C-c n T" . org-roam-dailies-goto-today)
    ("C-c n w" . org-roam-dailies-goto-tomorrow)
    ("C-c n y" . org-roam-dailies-goto-yesterday)
    (:map
      org-mode-map
      (("C-c n i" . org-roam-node-insert)
        ("C-c n o" . org-id-get-create)
        ("C-c n t" . org-roam-tag-add)
        ("C-c n a" . org-roam-alias-add)
        ("C-c n l" . org-roam-buffer-toggle)))))

;; Automatically save my org buffers
(use-package real-auto-save
  :custom (real-auto-save-interval 5)
  :hook (org-mode-hook . real-auto-save-mode))

(provide 'org)
;;; org.el ends here
