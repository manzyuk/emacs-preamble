(require 'paredit)

(autoload 'enable-paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(eval-after-load 'paredit
  '(define-key paredit-mode-map
     (kbd "C-<backspace>") 'paredit-backward-kill-word))

(provide 'preamble-paredit)
