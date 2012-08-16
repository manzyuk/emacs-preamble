;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)

;; Teach dired to uncompress zip files.
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(defadvice dired-do-async-shell-command (around dired-do-async-shell-command-silently activate)
  "Execute a shell command silently.

Don't display any output or errors and detach the command from
the Emacs process, so that it persists even if Emacs exits."
  (silently ad-do-it))

(defun preamble-dired-do-xdg-open ()
  "In Dired, open file mentioned on this line in user's preferred
application."
  (interactive)
  (dired-do-async-shell-command
   "xdg-open"
   nil
   (list (dired-get-filename t nil))))

(defun preamble-dired-hook ()
  (define-key dired-mode-map "\r"
    'dired-find-alternate-file)
  (define-key dired-mode-map "^"
    (lambda ()
      (interactive)
      (find-alternate-file "..")))
  (define-key dired-mode-map "V"
    'preamble-dired-do-xdg-open)
  (define-key dired-mode-map "E"
    'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook 'preamble-dired-hook)

(provide 'preamble-dired)
