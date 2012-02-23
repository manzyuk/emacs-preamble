(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar preamble-packages
  '(auctex
    ghci-completion
    google-translate
    haskell-mode
    inf-ruby
    magit
    org
    paredit)
  "List of package to ensure are installed at launch.")

(dolist (package preamble-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'preamble-package)
