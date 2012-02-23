;; Kill and copy text only if there is an active region (the default
;; behavior when C-w kills the region between the mark and the point
;; even if the region is not active is irritating).

(defun preamble-kill-region-if-mark-active ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (message "There is no active region")))

(defun preamble-kill-ring-save-if-mark-active ()
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (message "There is no active region")))

(global-set-key (kbd "C-w")        'preamble-kill-region-if-mark-active)
(global-set-key (kbd "S-<delete>") 'preamble-kill-region-if-mark-active)
(global-set-key (kbd "M-w")        'preamble-kill-ring-save-if-mark-active)
(global-set-key (kbd "C-<insert>") 'preamble-kill-ring-save-if-mark-active)

;; When I select something in another program to paste it into Emacs,
;; but kill something in Emacs before actually pasting it, don't lose
;; the selection; save it in the `kill-ring' before the Emacs kill so
;; that I can still paste it using C-y M-y.
(setq save-interprogram-paste-before-kill t)

;; Enable `delete-selection-mode'.
(delete-selection-mode 1)

;; Always add a new line at the end of the file.
(setq require-final-newline 'visit-save)

;; Enable `show-paren' mode.
(show-paren-mode 1)

;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

(provide 'preamble-editing)
