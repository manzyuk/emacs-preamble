;; Disable display of a menu bar, tool bar, and scroll bars on
;; all frames.  On Linux, these are normally turned off in
;; Xresources; the following is to make sure they are also off
;; on Windows.
(dolist (mode '(menu-bar-mode
                tool-bar-mode
                scroll-bar-mode
                blink-cursor-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; For buffers visiting files show the full file name in the title
;; bar; for buffers not associated with files show the buffer name.
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Visually indicate empty lines after the buffer end.
(setq-default indicate-empty-lines t)

;; Don't show a cursor except in the selected window.
(setq-default cursor-in-non-selected-windows nil)

;; Workaround for the problem of Emacs frame not being raised under X.
;; http://permalink.gmane.org/gmane.emacs.help/81708
(defadvice raise-frame (around wmctrl activate)
  (if (eq (window-system (ad-get-arg 0)) 'x)
      (x-send-client-message nil 0 (ad-get-arg 0)
                             "_NET_ACTIVE_WINDOW" 32 '(1))
    ad-do-it))

(provide 'preamble-frames)
