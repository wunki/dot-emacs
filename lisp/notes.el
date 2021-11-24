;;; notes.el --- Setup org and org-roam for note taking -*- lexical-binding: t -*-

;;; Code:
(setq org-directory (concat (getenv "HOME") "/dropbox/Notes/"))

(use-package org)

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(provide 'notes)
;;; notes.el ends here
