;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)

;; Teach dired to uncompress zip files.
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

;; Make `dired-do-async-shell-command' more useful: don't display any
;; output or errors and detach the command from the Emacs process, so
;; that it persists even if Emacs exits.
(defadvice dired-do-async-shell-command (around dired-do-async-shell-command-silently)
  (flet ((shell-command (command &optional output-buffer error-buffer)
           (preamble-shell-command-silently command)))
    ad-do-it))

(ad-activate 'dired-do-async-shell-command)

(provide 'preamble-dired)
