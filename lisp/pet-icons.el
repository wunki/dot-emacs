;;; pet-icons.el --- pretty icons everywhere -*- lexical-binding: t -*-
;;
;;; Commentary:
;;; Code:
;;

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :after (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'pet-icons)
;;; pet-icons.el ends here
