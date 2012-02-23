;; Make mouse middle-click only paste from primary X11 selection, not
;; from clipboard or `kill-ring'.
(global-set-key [mouse-2] 'mouse-yank-primary)

(provide 'preamble-mouse)
