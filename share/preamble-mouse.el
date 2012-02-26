;; Make mouse middle-click only paste from primary X11 selection, not
;; from clipboard or `kill-ring'.
(global-set-key [mouse-2] 'mouse-yank-primary)

;; Yank at point instead of at click.
(setq mouse-yank-at-point t)

(provide 'preamble-mouse)
