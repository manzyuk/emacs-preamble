(require 'shell)
(require 'ansi-color)
(require 'preamble-ansi-color)

;;; General settings

;; Do not highlight any additional expressions in shell mode.
(setq shell-font-lock-keywords nil)

;; Use the custom color theme.
(add-hook 'shell-mode-hook 'preamble-update-color-map)

;;; Directory tracking

;; More reliable shell directory tracking.  Unfortunately, Unix-only.
;; From https://github.com/nelhage/elisp/blob/master/dot-emacs.
(defun preamble-inferior-process-cwd (buffer)
  "Return the current working directory of the process associated
with BUFFER, if any, or nil otherwise."
  (let ((process (get-buffer-process buffer)))
    (when process
      (let ((pid (process-id process)))
        (file-symlink-p (format "/proc/%d/cwd" pid))))))

(defun preamble-shell-mode-cd (text)
  (let ((cwd (preamble-inferior-process-cwd (current-buffer))))
    (when cwd (cd cwd)))
  text)

(defun preamble-turn-on-directory-tracking ()
  "Enable smarter directory tracking.  Fall back to `dirtrack-mode'
on Windows."
  (shell-dirtrack-mode 0)
  (if (memq system-type (list 'ms-dos 'windows-nt 'cygwin))
      ;; On non-Unix systems, track directory by watching the prompt.
      (dirtrack-mode 1)
    (add-hook 'comint-preoutput-filter-functions
              'preamble-shell-mode-cd nil 'local)))

(add-hook 'shell-mode-hook
          'preamble-turn-on-directory-tracking)

(provide 'preamble-shell)
