(defun preamble-comment-auto-fill-only-comments ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode 1))

(defun preamble-prog-mode-hook ()
  (flyspell-prog-mode)
  (preamble-comment-auto-fill-only-comments))

(add-hook 'prog-mode-hook
          'preamble-prog-mode-hook)

(provide 'preamble-prog)
