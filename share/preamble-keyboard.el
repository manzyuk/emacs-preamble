;; Set the default coding system to UTF-8.
(prefer-coding-system 'utf-8-unix)

;; Unset `C-z', bound to `suspend-frame', whose behavior in xmonad is
;; confusing.  `suspend-frame' is still available through `C-x C-z'.
(global-unset-key "\C-z")

(provide 'preamble-keyboard)
