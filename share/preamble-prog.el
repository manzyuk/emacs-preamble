(defun preamble-comment-auto-fill-only-comments ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode 1))

(add-hook 'prog-mode-hook
          'preamble-comment-auto-fill-only-comments)

(provide 'preamble-prog)
