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

(use-feature lisp-mode
  :custom
  (lisp-indent-function 'common-lisp-indent-function))

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
  (sly-complete-symbol-function 'sly-flex-completions)
  :hook (lisp-mode . sly-editing-mode)
  :bind (:map sly-mode-map
              ("C-c C-z" . pet/sly-mrepl-dwim)
              ("C-c C-k" . sly-compile-and-load-file)
              ("C-c C-r" . sly-compile-region)
              ("C-c C-a l" . pet/sly-asdf-load-system)
              ("C-c C-a t" . pet/sly-asdf-test-system)
              ("C-c C-a r" . pet/sly-asdf-reload-system)
              ("C-c C-d d" . sly-describe-symbol)
              ("C-c C-d h" . sly-hyperspec-lookup))
  :preface
  (defvar pet/sly-source-buffer nil
    "Buffer to return to from the SLY REPL.")

  (defun pet/sly-project-root ()
    "Return the current project root, preferring Emacs project metadata."
    (when-let* ((project (project-current nil)))
      (project-root project)))

  (defun pet/sly-project-asdf-systems ()
    "Return ASDF system names in the current project root."
    (when-let* ((root (pet/sly-project-root)))
      (mapcar (lambda (file)
                (file-name-base file))
              (directory-files root nil "\\.asd\\'"))))

  (defun pet/sly-read-asdf-system ()
    "Read an ASDF system name, defaulting to the current project."
    (let* ((systems (pet/sly-project-asdf-systems))
           (default (car systems)))
      (cond
       ((null systems)
        (read-string "ASDF system: "))
       ((cdr systems)
        (completing-read "ASDF system: " systems nil t nil nil default))
       (t default))))

  (defun pet/sly-eval-string (form)
    "Evaluate FORM through SLY, starting SLY first if needed."
    (if (sly-connected-p)
        (sly-interactive-eval form)
      (call-interactively #'sly)
      (message "Started SLY; rerun the command once the REPL is connected.")))

  (defun pet/sly-asdf-form (operator system &optional keys)
    "Build an ASDF OPERATOR form for SYSTEM and KEYS."
    (format "(asdf:%s %S%s)"
            operator
            system
            (if keys
                (concat " " keys)
              "")))

  (defun pet/sly-asdf-load-system (system)
    "Load the current ASDF SYSTEM."
    (interactive (list (pet/sly-read-asdf-system)))
    (pet/sly-eval-string (pet/sly-asdf-form "load-system" system)))

  (defun pet/sly-asdf-test-system (system)
    "Run ASDF tests for the current SYSTEM."
    (interactive (list (pet/sly-read-asdf-system)))
    (pet/sly-eval-string (pet/sly-asdf-form "test-system" system)))

  (defun pet/sly-asdf-reload-system (system)
    "Force-reload the current ASDF SYSTEM."
    (interactive (list (pet/sly-read-asdf-system)))
    (pet/sly-eval-string (pet/sly-asdf-form "load-system" system ":force t")))

  (defun pet/sly-remember-source-buffer ()
    "Remember the current SLY source buffer for REPL toggling."
    (when (bound-and-true-p sly-mode)
      (setq pet/sly-source-buffer (current-buffer))))

  (defun pet/sly-pop-to-mrepl ()
    "Pop to the current SLY MREPL."
    (sly-mrepl 'pop-to-buffer))

  (defun pet/sly-pop-to-source-buffer ()
    "Pop to the source buffer remembered by `pet/sly-mrepl-dwim'."
    (when (buffer-live-p pet/sly-source-buffer)
      (pop-to-buffer pet/sly-source-buffer)))

  (defun pet/sly-mrepl-dwim ()
    "Start SLY, switch to the MREPL, or toggle back to source."
    (interactive)
    (cond
     ((derived-mode-p 'sly-mrepl-mode)
      (or (pet/sly-pop-to-source-buffer)
          (pet/sly-pop-to-mrepl)))
     ((not (sly-connected-p))
      (pet/sly-remember-source-buffer)
      (call-interactively #'sly))
     (t
      (pet/sly-remember-source-buffer)
      (pet/sly-pop-to-mrepl))))

  (defun pet/sly-bind-mrepl-dwim ()
    "Bind `pet/sly-mrepl-dwim' after SLY contribs have touched keymaps."
    (define-key sly-editing-mode-map (kbd "C-c C-z") #'pet/sly-mrepl-dwim)
    (define-key sly-mode-map (kbd "C-c C-z") #'pet/sly-mrepl-dwim)
    (define-key sly-mrepl-mode-map (kbd "C-c C-z") #'pet/sly-mrepl-dwim)))

(use-package sly-mrepl
  :ensure nil
  :after sly
  :custom
  (sly-mrepl-history-file-name
   (no-littering-expand-var-file-name "sly/mrepl-history"))
  (sly-mrepl-prevent-duplicate-history 'move)
  :hook (sly-mrepl-mode . electric-pair-mode)
  :config
  (pet/sly-bind-mrepl-dwim)
  (add-hook 'sly-connected-hook #'pet/sly-bind-mrepl-dwim))

(use-package sly-asdf
  :after sly
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(provide 'pet-lisp)
;;; pet-lisp.el ends here
