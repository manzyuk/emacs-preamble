(require 'shell)
(require 'preamble-ansi-color)

;;; General settings

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

(provide 'preamble-shell)
