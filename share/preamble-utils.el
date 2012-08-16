(defun preamble-regexp-alternatives (regexps)
  "Return a regexp matching anything that any of REGEXPS matches."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defun preamble-shell-command-to-list (command)
  "Execute shell command COMMAND and return its output as a list
of separate lines."
  (split-string (shell-command-to-string command) "\n" t))

(defun preamble-reload-files ()
  "Revisit files loaded into the current Emacs session."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((file-name (buffer-file-name buffer)))
      (when file-name
        (kill-buffer buffer)
        (find-file file-name)))))

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

(defun preamble-xdg-open (filename)
  "Open file FILENAME in user's preferred application."
  (interactive
   (list (ido-read-file-name "Open file: ")))
  (async-shell-command
   (format "xdg-open \"%s\"" filename)))

(defun preamble-kill-autoload-buffers ()
  "Kill `-autoload.el' buffers loaded by `package'."
  (dolist (buffer (buffer-list))
    (when (string-match "-autoloads\\.el\\'" (buffer-name buffer))
      (kill-buffer buffer))))

(provide 'preamble-utils)
