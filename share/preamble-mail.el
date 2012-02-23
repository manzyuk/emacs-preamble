;; My full name and email.
(setq user-full-name    "Oleksandr Manzyuk")
(setq user-mail-address "manzyuk@gmail.com")

(require 'smtpmail)

;; Enable sending email from Emacs using my GMail account.
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(provide 'preamble-mail)
