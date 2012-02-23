(defun preamble-makefile-mode-hook ()
  ;; Don't use spaces instead of tabs.
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook
          'preamble-makefile-mode-hook)

(provide 'preamble-makefile)
