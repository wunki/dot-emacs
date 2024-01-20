;;; early-init.el --- the earliest inits -*- lexical-binding: t -*-
;;
;;; Commentary:
;; 
;; We put code here that we want to have available first.
;;
;;; Code:
;;

(provide 'early-init)

(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/13:/opt/homebrew/opt/libgccjit/lib/gcc/13:/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin23/13")

;; Native compiled cache, complies with the `no-littering' package.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;;; early-init.el ends here
