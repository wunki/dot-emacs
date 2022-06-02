;;; packages.el --- package management with straight -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is where we setup our package management by using straight
;; and the excellent use-package library.
;;
;;; Code:
;;

;; Setup straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable package.el in favor of straight
(setq package-enable-at-startup nil)

;; Install use-package
(straight-use-package 'use-package)

;; Make sure the use-package macro's are available
(eval-and-compile (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Configure use-package to use straight.el by default
(use-package straight
  :ensure nil
  :custom (straight-use-package-by-default t))

;; Clean up the modeline
(use-package delight)

(provide 'packages)
;;; packages.el ends here
