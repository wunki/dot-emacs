;;; pet-pet-mail.el --- sending mail with emacs and Mu4e -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;
;;; To be able to use the library, you need to have the mu command available.
;;;
;;; Code:

;; Emacs email configuration
(setq mail-user-agent 'mu4e-user-agent
      ;; don't keep message buffers around
      message-kill-buffer-on-exit t
      ;; default to smtpmail for sending email
      message-send-mail-function 'smtpmail-send-it)

(use-package mu4e
  :ensure nil
  :straight (:host github
             :files ("build/mu4e/*.el")
             :repo "djcb/mu"
             :pre-build (("./autogen.sh")
                         ("ninja" "-C" "build")))
  :commands mu4e
  :bind (("C-c m" . mu4e)
         :map mu4e-headers-mode-map
         ("A" . mu4e-headers-mark-for-archive))
  :config
  (add-to-list 'mu4e-marks
               '(archive
                 :char       "A"
                 :prompt     "Archive"
                 :show-target (lambda (target) "archive")
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg "-\\Inbox")
                                (mu4e~proc-move docid nil "+S-u-N"))))
  (add-to-list 'mu4e-bookmarks
              '(:name  "Workera's Inbox"
                :query "maildir:\"/workera/inbox\""
                :key   ?d))
  (add-to-list 'mu4e-bookmarks
               '(:name  "Home's Inbox"
                 :query "maildir:\"/home/inbox\""
                 :key   ?p))
  (mu4e~headers-defun-mark-for archive)
  ;; mu4e configuration
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-change-filenames-when-moving t
        mu4e-maildir "~/mail"
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil
        mu4e-use-fancy-chars t
        mu4e-confirm-quit nil
        mu4e-headers-skip-duplicates t
        mu4e-html2text-command "html2text")
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Home"
             :enter-func (lambda () (mu4e-message "Entering home context"))
             :leave-func (lambda () (mu4e-message "Leaving home context"))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/petar" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address      . "petar@petar.dev")
                     (user-full-name         . "Petar Radosevic")
                     (mu4e-sent-folder       .  "/home/sent")
                     (mu4e-trash-folder      .  "/home/trash")
                     (mu4e-refile-folder     .  "/home/archive")
                     (mu4e-drafts-folder     .  "/home/drafts")
                     (mu4e-sent-messages-behavior . sent)
                     (mu4e-compose-signature . (concat
                                                "Petar Radosevic\n"
                                                "petar@petar.dev | www.petar.dev"))
                     (starttls-use-gnutls           . t)
                     (smtpmail-starttls-credentials . '(("smtp.fastmail.com" 587 nil nil)))
                     (smtpmail-default-smtp-server . "smtp.fastmail.com")
                     (smtpmail-smtp-server . "smtp.fastmail.com")
                     (smtpmail-smtp-user . "wunki@fastmail.fm")))
           ,(make-mu4e-context
             :name "Workera"
             :enter-func (lambda () (mu4e-message "Entering Workera context"))
             :leave-func (lambda () (mu4e-message "Leaving Workera context"))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/workera" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address      . "petar@workera.ai")
                     (user-full-name         . "Petar Radosevic")
                     (mu4e-snent-folder       .  "/workera/sent")
                     (mu4e-trash-folder      .  "/workera/trash")
                     (mu4e-refile-folder     .  "/workera/archive")
                     (mu4e-drafts-folder     .  "/workera/drafts")
                     (mu4e-sent-messages-behavior . delete)
                     (mu4e-compose-signature . (concat
                                                "Petar Radošević — VP Technology\n"
                                                "petar@workera.ai | Timezone: CEST"))
                     (starttls-use-gnutls           . t)
                     (smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)))
                     (smtpmail-default-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-service . 587)
                     (smtpmail-smtp-user . "petar@workera.ai"))))))

(provide 'pet-mail)
;;; pet-mail.el ends here
