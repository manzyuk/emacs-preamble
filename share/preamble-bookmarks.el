;; Dumbed down, quick, nameless bookmarks.  One could also use marks
;; instead of bookmarks (set a mark with C-SPC C-SPC and jump to a
;; mark with C-u C-SPC or C-x C-SPC), but marks are modified by long
;; movement commands.
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

(setq bookmark-default-file
      (concat user-emacs-directory "bookmarks"))

(provide 'preamble-bookmarks)
