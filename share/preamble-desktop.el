;; Enable `desktop-save-mode'.
(setq desktop-base-file-name "desktop"
      desktop-base-lock-name "desktop.lock"
      desktop-dirname user-emacs-directory)
(desktop-save-mode 1)

(provide 'preamble-desktop)
