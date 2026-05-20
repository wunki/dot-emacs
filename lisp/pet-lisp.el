;;; pet-lisp.el --- Common Lisp configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)

;; projects don't need to be added to Git to be found
(defun pet/project-find-asdf-system (dir)
  "Find Common Lisp ASDF project root for DIR."
  (when-let* ((root (locate-dominating-file
                     dir
                     (lambda (directory)
                       (directory-files directory nil "\\.asd\\'")))))
    (cons 'asdf-system root)))

(cl-defmethod project-root ((project (head asdf-system)))
  "Return root of ASDF system PROJECT."
  (cdr project))

(add-hook 'project-find-functions #'pet/project-find-asdf-system)

(use-package paredit
  :hook (lisp-mode . paredit-mode))

;; open up Hyperspec urls in Emacs itself
(use-feature browse-url
  :config
  (add-to-list
   'browse-url-handlers
   '("https://www\\.lispworks\\.com/reference/HyperSpec/" . eww-browse-url)))

(use-package sly
  :init
  (setq sly-contribs '(sly-fancy
                       sly-asdf
                       sly-profiler
                       sly-trace-dialog
                       sly-stickers))
  :custom
  (inferior-lisp-program "sbcl")
  (sly-lisp-implementations '((sbcl ("sbcl"))))
  :bind (:map sly-mode-map
              ("C-c C-k" . sly-compile-and-load-file)
              ("C-c C-r" . sly-compile-region)
              ("C-c C-d d" . sly-describe-symbol)
              ("C-c C-d h" . sly-hyperspec-lookup))
  :preface
  (defvar pet/sly-source-buffer nil
    "Buffer to return to from the SLY REPL.")

  (defun pet/sly-mrepl-toggle-around (fn &rest args)
    "Make `sly-mrepl' toggle back to the last source buffer.
FN and ARGS are the original `sly-mrepl' function and arguments."
    (if (derived-mode-p 'sly-mrepl-mode)
        (if (buffer-live-p pet/sly-source-buffer)
            (pop-to-buffer pet/sly-source-buffer)
          (apply fn args))
      (when (bound-and-true-p sly-mode)
        (setq pet/sly-source-buffer (current-buffer)))
      (apply fn args))))

(use-package sly-mrepl
  :ensure nil
  :after sly
  :custom
  (sly-mrepl-history-file-name
   (no-littering-expand-var-file-name "sly/mrepl-history"))
  (sly-mrepl-prevent-duplicate-history 'move)
  :hook (sly-mrepl-mode . electric-pair-mode)
  :config
  ;; Keep SLY's own C-c C-z binding, but make repeated use toggle back.
  (advice-add 'sly-mrepl :around #'pet/sly-mrepl-toggle-around))

(use-package sly-asdf
  :after sly
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(provide 'pet-lisp)
;;; pet-lisp.el ends here
