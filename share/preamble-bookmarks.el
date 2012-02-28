(defun preamble-bookmark-set ()
  (interactive)
  (bookmark-set (buffer-name)))

(defun preamble-bookmark-jump ()
  (interactive)
  (let ((name (buffer-name)))
    (if (assoc name bookmark-alist)
        (bookmark-jump name)
      (message "No bookmark set"))))

(global-set-key "\C-cm" 'preamble-bookmark-set)
(global-set-key "\C-cj" 'preamble-bookmark-jump)

(provide 'preamble-bookmarks)
