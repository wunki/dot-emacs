;;; early-init.el --- the earliest inits -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(provide 'early-init)

;; Native compiled cache, complies with the `no-littering' package.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; We use built-in package.el + use-package.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
