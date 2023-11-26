;;; lib.el --- my utulity functions -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Add helpful functions to our Emacs.  All functions should start
;; with `pet' so we can easily find them.
;; 
;;; Code:
;;

(require 'packages)
(require 'ert)

(use-package s
  :commands (s-trim s-concat))

(defun pet/kill-region-or-backward-word ()
  "Kill the active region if it exists, otherwise kill the word backwards."
  (interactive)
  (if (region-active-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun pet/edit-emacs-configuration ()
  "Edit a file in my Emacs configuration."
  (interactive)
  (let ((project-root "~/.config/emacs"))
    (if (and project-root (file-directory-p project-root))
        (let ((default-directory project-root))
          (call-interactively 'project-find-file))
      (message "Emacs configuration not found at: %s" project-root))))

(defun pet/is-mac ()
  "Return non-nil if running on a mac."
  (eq system-type 'darwin))

(defun pet/is-bsd ()
  "Return non-nil if running on FreeBSD."
  (eq system-type 'berkeley-unix))

(defun pet/is-wsl ()
  "Return non-nil if running on Windows WSL."
  (and (string-equal system-type 'gnu/linux) (getenv "WSLENV")))

(defun pet/is-linux ()
  "Return non-nil if running on Linux."
  (and (not (pet/is-wsl)) (string-equal system-type "gnu/linux")))

(defun pet/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
        (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun pet/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun pet/random-theme ()
  "Randomly change the theme."
  (interactive)
  (let* ((themes (custom-available-themes))
         (random-theme (nth (random (length themes)) themes)))
    (message "Current theme: %s" random-theme)
    (load-theme random-theme t)))

(defun pet/eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file.
The eshell is renamed to match that directory to make multiple eshell
windows easier."
  (interactive)
  (let*
    (
      (parent
        (if (buffer-file-name)
          (file-name-directory (buffer-file-name))
          default-directory))
      (height (/ (window-total-height) 3))
      (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

(defun pet/url-get-title (url &optional)
  "Takes a URL and return the value of the <title> HTML tag."
  (let ((buffer (url-retrieve-synchronously url))
        (title nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (search-forward-regexp "<title>\\([^<]+?\\)</title>")
      (setq title (match-string 1 ) )
      (kill-buffer (current-buffer)))
    title))
(setq org-make-link-description-function 'pet/url-get-title)

(defun eshell/q ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun pet/set-font (font)
  "Take a FONT and set it.
If the window-system is active, it directly changes the font.
Otherwise it adds it to the so it works with the Emacs daemon."
  (interactive "sFont: ")
  (if (window-system)
      (set-frame-font font nil t)
    (add-to-list 'default-frame-alist `(font . ,font))))

(defun pet/eval-and-run-all-tests-in-buffer ()
  "Clear and run all test in the current buffer."
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert 't))

(defun pet/reload-emacs ()
  "Reset Emacs to a clean state."
  (interactive)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (delete-other-windows)
  (load-file user-init-file))

(defun pet/current-project-root ()
  "Return the root directory of the current project."
  (when-let ((project (project-current)))
    (nth 2 project)))

(defun pet/insert-project-note ()
  "Insert a note for the current project."
  (interactive)
  (let* ((project-root (pet/current-project-root))
         (notes-file (concat project-root "NOTES.org"))
         (today (format-time-string "%Y-%m-%d %a"))
         (today-header (concat "** <" today ">")))
    (find-file notes-file)
    (goto-char (point-min))
    (if (search-forward today-header nil t)
        (progn
          (org-end-of-subtree)
          (newline))
      (progn
        (goto-char (point-max))
        (insert (concat "\n" today-header "\n"))))))

(provide 'lib)
;;; lib.el ends here
