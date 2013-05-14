;; Enable `icomplete-mode'.
(icomplete-mode 1)

;;; Use y/n instead of yes/no in confirmation dialogs.
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable `savehist-mode'.  Keep track of search entries.
(setq savehist-additional-variables '(search-ring regexp-search-ring))
(savehist-mode 1)

;; Enable `paredit-mode' in the minibuffer during `eval-expression'.
(eval-after-load "paredit"
  '(progn
     (defun preamble-turn-on-paredit-mode-in-minibuffer ()
       (when (eq this-command 'eval-expression)
         (paredit-mode 1)))
     (add-hook 'minibuffer-setup-hook
               'preamble-turn-on-paredit-mode-in-minibuffer)))

(provide 'preamble-minibuffer)
