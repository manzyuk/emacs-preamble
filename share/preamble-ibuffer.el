;; Replace standard buffer menu with `ibuffer'.
(global-set-key "\C-x\C-b" 'ibuffer)

;; Define some filter groups.
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired"
                (mode . dired-mode))
               ("Emacs"
                (or
                 (mode . help-mode)
                 (mode . Info-mode)
                 (mode . shell-mode)
                 (mode . Custom-mode)
                 (mode . apropos-mode)
                 (mode . emacs-lisp-mode)
                 (mode . completion-list-mode)
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*Messages\\*$")))
               ("Org"
                (or
                 (mode . org-mode)
                 (mode . org-agenda-mode)))
               ("LaTeX"
                (or
                 (mode . latex-mode)
                 (mode . bibtex-mode)))
               ("Scheme"
                (or
                 (mode . scheme-mode)
                 (mode . inferior-scheme-mode)))
               ("Haskell"
                (or
                 (mode . haskell-mode)
                 (mode . literate-haskell-mode)
                 (mode . inferior-haskell-mode)))))))

(defun preamble-ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook
          'preamble-ibuffer-mode-hook)

(provide 'preamble-ibuffer)
