;;; notes.el --- Setup org and org-roam for note taking -*- lexical-binding: t -*-

;;; Code:
(setq org-directory (concat (getenv "HOME") "/Notes/"))

(use-package org
  :config
  (setq org-return-follows-link t))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n w" . org-roam-dailies-goto-tomorrow)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle))))
  :config
  (setq org-startup-indented t)
  (setq org-roam-dailies-directory "Dailies/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")))))

(provide 'notes)
;;; notes.el ends here
