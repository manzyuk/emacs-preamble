(require 'magit)
(require 'preamble-diff)

(set-face-attribute 'magit-item-highlight nil :inherit nil)

(global-set-key "\C-cg" 'magit-status)

(provide 'preamble-magit)
