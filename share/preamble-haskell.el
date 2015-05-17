(require 'preamble-comint)
(require 'preamble-utils)

(autoload 'haskell-mode "haskell-mode" "Haskell mode." t)

(setq haskell-indent-offset 2)

(defun preamble-haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (inf-haskell-mode 1)
  (subword-mode 1))

(add-hook 'haskell-mode-hook
          'preamble-haskell-mode-hook)

(set-default 'haskell-literate-default 'tex)

(defun preamble-inferior-haskell-mode-hook ()
  (preamble-turn-on-comint-history)
  (subword-mode 1))

(add-hook 'inferior-haskell-mode-hook
          'preamble-inferior-haskell-mode-hook)

(provide 'preamble-haskell)
