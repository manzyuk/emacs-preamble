(require 'preamble-comint)
(require 'preamble-paredit)

(setq scheme-program-name "mit-scheme")

;; Enable comint input ring reading/writing from/to a history file.
(add-hook 'inferior-scheme-mode-hook
          'preamble-turn-on-comint-history)

(add-hook 'scheme-mode-hook
          'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook
          'enable-paredit-mode)

(provide 'preamble-scheme)
