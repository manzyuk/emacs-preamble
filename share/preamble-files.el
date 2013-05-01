;; Enable `global-auto-revert-mode'.
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t)

;; Don't make backup files.
(setq-default make-backup-files nil)

;; Uniquify buffer names with parts of directory name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Don't request confirmation before visiting a new file or buffer.
(setq confirm-nonexistent-file-or-buffer nil)

;; Clean up whitespace in the buffer on save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Automatically make scripts starting with #! executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'preamble-files)
