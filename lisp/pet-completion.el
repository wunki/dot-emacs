;;; pet-completion.el --- read my mind please -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Useful packages to read my mind and complete my sentences.
;; 
;;; Code:
;;

;; Popup completion-at-point
(use-package corfu
  :demand t
  :ensure '(corfu
            :host github
            :repo "minad/corfu"
            :files ("*" "extensions/*.el" (:exclude ".git")))
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

(provide 'pet-completion)
;;; pet-completion.el ends here
