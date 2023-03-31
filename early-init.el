;;; early-init.el --- the earliest inits -*- lexical-binding: t -*-
;;
;;; Commentary:
;; 
;; We put code here that we want to have available first.
;;
;;; Code:
;;

(when (file-directory-p "/opt/homebrew")
   (setenv
    "LIBRARY_PATH"
    (string-join
     '
     ("/opt/homebrew/opt/gcc/lib/gcc/current"
      "/opt/homebrew/opt/libgccjit/lib/gcc/current"
      "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin22/12")
     ":")))

(provide 'early-init)
;;; early-init.el ends here
