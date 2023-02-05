;;; early-init.el --- the earliest inits -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; On M1 macs, the GCC libraries need to be added to the so
;; we can make use of native compilation.
;;
;;; Code:
;;
(eval-when-compile (require 'subr-x))

;; TODO: check the system name to set the architecture
;; to Darwin 21 (M1) or Darwin 22 (M2).
(when (file-directory-p "/opt/homebrew")
  (setenv
   "LIBRARY_PATH"
   (string-join
    '
    ("/opt/homebrew/opt/gcc/lib/gcc/12"
     "/opt/homebrew/opt/libgccjit/lib/gcc/12"
     "/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin22/12")
    ":")))

(provide 'early-init)
;;; early-init.el ends here
