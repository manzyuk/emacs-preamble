(require 'preamble-paredit)

;; If a .el file is saved, it is likely that the corresponding .elc
;; file is no longer valid.
(defun preamble-delete-elc-on-save ()
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (let ((elc (concat buffer-file-name "c")))
                (when (file-exists-p elc)
                  (delete-file elc))))))

(defun preamble-emacs-lisp-mode-hook ()
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (preamble-delete-elc-on-save))

(add-hook 'emacs-lisp-mode-hook
          'preamble-emacs-lisp-mode-hook)

(defun preamble-ielm-mode-hook ()
  (enable-paredit-mode)
  (turn-on-eldoc-mode))

(add-hook 'ielm-mode-hook
          'preamble-ielm-mode-hook)

(provide 'preamble-emacs-lisp)
