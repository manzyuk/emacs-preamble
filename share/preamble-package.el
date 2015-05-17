(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar preamble-packages
  '(auctex haskell-mode magit paredit)
  "List of package to ensure are installed at launch.")

(dolist (package preamble-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'preamble-package)
