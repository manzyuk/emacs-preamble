(require 'preamble-ansi-color)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added preamble-green)
     (set-face-foreground 'diff-removed preamble-red)))

(provide 'preamble-diff)
