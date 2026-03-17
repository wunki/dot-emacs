;;; pet-completion.el --- in-buffer completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :demand t
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  (corfu-auto nil)
  :config
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Extra completion-at-point sources
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'pet-completion)
;;; pet-completion.el ends here
