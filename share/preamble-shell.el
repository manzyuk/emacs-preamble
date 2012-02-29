(require 'shell)
(require 'preamble-ansi-color)

;; Do not highlight any additional expressions in shell mode.
(setq shell-font-lock-keywords nil)

;; Use the custom color theme.
(add-hook 'shell-mode-hook 'preamble-update-color-map)

;; Track the current working directory by watching the prompt.
(defun preamble-turn-on-dirtrack-mode ()
  (dirtrack-mode 1))

(add-hook 'shell-mode-hook
          'preamble-turn-on-dirtrack-mode)

;; Use bash when requesting inferior shells (also remote ones).
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)

;; The standard way to have multiple shells is to open a shell, rename
;; its buffer, open a new shell, rename its buffer etc.  This is
;; tedious.  The following function makes it easier.
(defun preamble-named-shell (name directory)
  "Open a named shell. NAME is the base name of the shell buffer,
and DIRECTORY is the directory to open the shell in."
  (interactive "MName: \nDDirectory: ")
  (switch-to-buffer (concat "*" name "*"))
  (cd directory)
  (shell (current-buffer)))

(provide 'preamble-shell)
