(require 'preamble-comint)

(autoload 'ruby-mode "ruby-mode" "Ruby mode." t)

(add-to-list 'interpreter-mode-alist
             '("ruby" . ruby-mode))

(require 'inf-ruby)

(setq ruby-program-name "irb --inf-ruby-mode")

;; Enable comint input ring reading/writing from/to a history file.
(add-hook 'inferior-ruby-mode-hook
          'preamble-turn-on-comint-history)

(provide 'preamble-ruby)
