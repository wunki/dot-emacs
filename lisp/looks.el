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

(use-package nano-emacs
  :no-require t
  :straight (:host github :repo "rougier/nano-emacs")
  :defines (nano-font-family-monospaced nano-font-size)
  :functions (nano-toggle-theme)
  :bind (("C-c C-n" . nano-toggle-theme))
  :config
  (setq nano-font-family-monospaced "Berkeley Mono")
  (setq nano-font-size 15)
  (require 'nano))

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
