(require 'preamble-comint)

(autoload 'ruby-mode "ruby-mode" "Ruby mode." t)

(add-to-list 'interpreter-mode-alist
             '("ruby" . ruby-mode))

(require 'inf-ruby)

(setq ruby-program-name "irb --inf-ruby-mode")

(add-hook 'inferior-ruby-mode-hook
          'preamble-turn-on-comint-history)

(provide 'preamble-ruby)
