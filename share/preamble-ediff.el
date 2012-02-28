;; Do everything in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Split windows horizontally rather than vertically.
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'preamble-ediff)
