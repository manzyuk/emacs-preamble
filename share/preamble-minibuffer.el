;; Enable `icomplete-mode'.
(icomplete-mode 1)

;;; Use y/n instead of yes/no in confirmation dialogs.
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable `savehist-mode'.  Keep track of search entries.
(setq savehist-additional-variables '(search-ring regexp-search-ring))
(savehist-mode 1)

(provide 'preamble-minibuffer)
