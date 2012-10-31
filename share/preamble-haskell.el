(require 'preamble-comint)
(require 'preamble-utils)

(load "haskell-site-file.el")

(autoload 'haskell-mode "haskell-mode" "Haskell mode." t)

(setq haskell-indent-offset 2)

(defun preamble-haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (subword-mode 1))

(add-hook 'haskell-mode-hook
          'preamble-haskell-mode-hook)

(set-default 'haskell-literate-default 'tex)

(require 'ghci-completion)

(defun preamble-inferior-haskell-mode-hook ()
  (preamble-turn-on-comint-history)
  (turn-on-ghci-completion)
  (subword-mode 1))

(add-hook 'inferior-haskell-mode-hook
          'preamble-inferior-haskell-mode-hook)

(provide 'preamble-haskell)
