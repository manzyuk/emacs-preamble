(require 'magit)

(global-set-key "\C-cg" 'magit-status)

;; Use colors consistent with gnome-terminal with Tango color theme.
(set-face-foreground 'magit-diff-del preamble-red)
(set-face-foreground 'magit-diff-add preamble-green)
(set-face-foreground 'magit-diff-hunk-header preamble-cyan)

;; Don't italize hunk headers.
(set-face-italic-p   'magit-diff-hunk-header nil)

(provide 'preamble-magit)
