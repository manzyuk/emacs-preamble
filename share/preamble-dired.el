;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)

;; Teach dired to uncompress zip files.
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(defadvice dired-do-async-shell-command (around dired-do-async-shell-command-silently)
  "Execute a shell command silently.

Don't display any output or errors and detach the command from
the Emacs process, so that it persists even if Emacs exits."
  (flet ((shell-command (command &optional output-buffer error-buffer)
           (preamble-shell-command-silently command))
         (message (format-string &rest args)
           nil))
    ad-do-it))

(ad-activate 'dired-do-async-shell-command)

(provide 'preamble-dired)
