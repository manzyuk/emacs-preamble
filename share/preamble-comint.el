(require 'comint)

;;; General settings

;; Don't add input matching the last on the input ring.
(setq comint-input-ignoredups t)

;; Make the comint prompt read only.
(setq comint-prompt-read-only t)

;; Interpreter output moves point to the end of the output, but only
;; in the selected window so that we can simultaneously look at
;; previous outputs in other windows.
(setq-default comint-move-point-for-output 'this)

;; Input to interpreter causes the selected window to scroll.
(setq-default comint-scroll-to-bottom-on-input 'this)

;;; Enable reading/writing of comint input ring from/to a history file

(defun preamble-comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (insert (format "\nProcess %s %s" process event))))))

(defun preamble-turn-on-comint-history ()
  "Enable reading/writing of comint input ring from/to a history file."
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (concat user-emacs-directory
                    (format "inferior-%s-history"
                            (process-name process))))
      (comint-read-input-ring t)
      (set-process-sentinel process
                            #'preamble-comint-write-history-on-exit))))

;; If the buffer associated with a process is killed, the process's
;; sentinel is invoked when buffer-local variables  (in particular,
;; `comint-input-ring-file-name' and `comint-input-ring') are gone.
;; Therefore try to save the history every time a buffer is killed.
(add-hook 'kill-buffer-hook
          'comint-write-input-ring)

;; Apparently, when Emacs is killed, `kill-buffer-hook' is not run
;; on individual buffers.  We circumvent that by adding a hook to
;; `kill-emacs-hook' that walks the list of all buffers and writes
;; the input ring (if it is available) of each buffer to a file.

(defun preamble-comint-write-input-ring-all-buffers ()
  "Walk the list of all buffers and write the input ring (if it is
available) of each buffer to a history file."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (comint-write-input-ring))))

(add-hook 'kill-emacs-hook
          'preamble-comint-write-input-ring-all-buffers)

(provide 'preamble-comint)
