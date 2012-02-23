(defvar preamble-directory (file-name-directory load-file-name)
  "The root directory of Emacs Preamble.")

(defvar preamble-local-directory (concat preamble-directory "local/")
  "The directory of local (host-specific) customizations.

All Emacs Lisp files there are loaded automatically.")

(defvar preamble-share-directory (concat preamble-directory "share/")
  "The directory of customizations shared between different hosts.")

(add-to-list 'load-path preamble-local-directory)
(add-to-list 'load-path preamble-share-directory)

(require 'preamble-utils)

;;; General customizations

(require 'preamble-browse-url)
(require 'preamble-data)
(require 'preamble-editing)
(require 'preamble-env)
(require 'preamble-files)
(require 'preamble-frames)
(require 'preamble-initialization)
(require 'preamble-keyboard)
(require 'preamble-minibuffer)
(require 'preamble-mode-line)
(require 'preamble-mouse)
(require 'preamble-server)
(require 'preamble-tramp)
(require 'preamble-windows)

;;; Built-in modes

(require 'preamble-ansi-color)
(require 'preamble-comint)
(require 'preamble-dired)
(require 'preamble-ibuffer)
(require 'preamble-ido)
(require 'preamble-mail)
(require 'preamble-makefile)
(require 'preamble-prog)
(require 'preamble-shell)
(require 'preamble-text)

;;; Extensions

(require 'preamble-package)

(require 'preamble-auctex)
(require 'preamble-emacs-lisp)
(require 'preamble-google-translate)
(require 'preamble-haskell)
(require 'preamble-lisp)
(require 'preamble-magit)
(require 'preamble-org)
(require 'preamble-ruby)
(require 'preamble-scheme)

(when (file-exists-p preamble-local-directory)
  (mapc 'load (directory-files preamble-local-directory nil "^[^#].*\\.el$")))

(add-hook 'after-init-hook 'preamble-kill-autoload-buffers)
