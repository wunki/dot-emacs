;;; ai.el --- pairing up with an AI -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is where I augment Emacs with AI.
;; 
;;; Code:
;;

(use-package copilot
  :straight (:host github
             :repo "zerolfx/copilot.el"
             :files ("dist" "*.el"))
  
  :preface
  (defun pet/copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
	    (indent-for-tab-command)))

  :bind (:map copilot-mode-map
              ("<tab>" . pet/copilot-tab)
              ("s-n" . copilot-next-completion)
              ("s-p" . copilot-previous-completion)
              ("s-w" . copilot-accept-completion-by-word)
              ("s-l" . copilot-accept-completion-by-line)))

(use-package gptel
  :custom
  ;; we don't stream as it seems to break on my machine.
  (gptel-stream nil)
  (gptel-default-mode 'org-mode))

(provide 'ai)
;;; ai.el ends here
