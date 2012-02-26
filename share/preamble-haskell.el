(require 'preamble-comint)
(require 'preamble-prog)
(require 'preamble-utils)

(load "haskell-site-file.el")

(autoload 'haskell-mode "haskell-mode" "Haskell mode." t)

(defun preamble-haskell-mode-hook ()
  (preamble-prog-mode-hook)             ; because `haskell-mode' is
                                        ; not derived from `prog-mode'
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (subword-mode 1))

(add-hook 'haskell-mode-hook
          'preamble-haskell-mode-hook)

(set-default 'haskell-literate-default 'tex)

(require 'ghci-completion)

(defun preamble-inferior-haskell-mode-hook ()
  ;; Enable comint input ring reading/writing from/to a history file.
  (preamble-turn-on-comint-history)
  ;; Enable completion for GHCi commands in inferior-haskell buffers.
  (turn-on-ghci-completion))

(add-hook 'inferior-haskell-mode-hook
          'preamble-inferior-haskell-mode-hook)

(provide 'preamble-haskell)
