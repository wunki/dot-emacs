;;; pet-icons.el --- pretty icons everywhere -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-packages)

(use-package nerd-icons)

(use-package nerd-icons-corfu
  :functions (nerd-icons-corfu-formatter)
  :defines (corfu-margin-formatters)
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :functions (nerd-icons-completion-mode)
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode 1)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'pet-icons)
;;; pet-icons.el ends here
