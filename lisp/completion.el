;;; completion.el --- read my mind please -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Useful packages to read my mind and complete my sentences.
;; 
;; 
;;; Code:
;;

;; Popup completion-at-point
(use-package corfu
  :elpaca '(corfu
            :host github
            :repo "minad/corfu"
            :files ("*" "extensions/*.el" (:exclude ".git")))
  :init
  (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  (corfu-auto nil)
  :config
  (corfu-popupinfo-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Improve the completions by removing order?
(use-package orderless
  :init
  (setq
    completion-styles
    '(orderless)
    completion-category-defaults
    nil
    completion-category-overrides
    '((file (styles partial-completion)))))

;; Pretty icons for corfu
(use-package kind-icon
  :if window-system
  :defines kind-icon-margin-formatter
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))



(provide 'completion)
;;; completion.el ends here
