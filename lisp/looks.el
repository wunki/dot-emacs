;;; looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; We want Emacs to look nice, with as minimal distraction as possible.
;; That's why we use the Nano theme from rougier.
;; 
;;; Code:
;;
(require 'lib)

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(setq nano-font-family-monospaced "Berkeley Mono")
(setq nano-font-size 14)
(require 'nano)

;; Easily scale the font size up and down
(use-package default-text-scale
  :demand
  :commands default-text-scale-mode
  :config (default-text-scale-mode))

(use-package rainbow-mode
  :commands rainbow-mode
  :diminish
  :hook ((web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(provide 'looks)
;;; looks.el ends here
