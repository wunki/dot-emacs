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
  :straight '(corfu :host github
                    :repo "minad/corfu"
                    :files ("*" "extensions/*.el" (:exclude ".git")))
  :init
  (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(provide 'completion)
;;; completion.el ends here
