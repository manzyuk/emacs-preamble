(defun preamble-regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

;; Copy the function definition of the symbol `shell-command', so that
;; we can dynamically shadow it with the help of `flet' and use the
;; old definition in the body of the new one without entering a loop.
(fset 'preamble-shell-command (symbol-function 'shell-command))

(defun preamble-shell-command-silently (command)
  "Execute string COMMAND in a subshell of inferior shell.  Don't
display any output or errors."
  (preamble-shell-command (format "( %s ) > /dev/null 2>&1" command)))

(defmacro silently (&rest body)
  "Execute BODY suppressing Emacs messages and any shell command
output and errors."
  `(flet ((shell-command (command &optional output-buffer error-buffer)
            (preamble-shell-command-silently command))
          (message (format-string &rest args)
            nil))
     ,@body))

(defadvice async-shell-command (around async-shell-command-silently activate)
  "Execute a shell command silently.

Don't display any output or errors and detach the command from
the Emacs process, so that it persists even if Emacs exits."
  (silently ad-do-it))

(defun preamble-kill-autoload-buffers ()
  "Kill `-autoload.el' buffers loaded by `package'."
  (dolist (buffer (buffer-list))
    (when (string-match "-autoloads\\.el\\'" (buffer-name buffer))
      (kill-buffer buffer))))

(provide 'preamble-utils)
