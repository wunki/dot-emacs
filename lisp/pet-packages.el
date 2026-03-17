;;; pet-packages.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs 30 ships with package.el and use-package built-in.
;; No external package manager needed.
;;
;;; Code:

(require 'package)
;; HTTPS protects transport. MELPA is unsigned anyway.
(setq package-check-signature nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; On first launch, refresh archives so packages can be installed.
(unless package-archive-contents
  (package-refresh-contents))

;; use-package: auto-install missing packages
(setq use-package-always-ensure t)

;; Convenience macro for built-in features
(defmacro use-feature (name &rest args)
  "Like `use-package' but for built-in features.
NAME and ARGS are passed to `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(use-package diminish)

(provide 'pet-packages)
;;; pet-packages.el ends here
