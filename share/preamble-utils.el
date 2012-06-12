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

(defun preamble-shell-command-silently (command)
  "Execute string COMMAND in a subshell of inferior shell.  Don't
display any output or errors."
  (shell-command (format "( %s ) > /dev/null 2>&1" command)))

(defun preamble-async-shell-command (command)
  "Execute string COMMAND asynchronously in background.

Unlike `async-shell-command', don't display any output or errors.
Furthermore, because COMMAND is executed in a subshell, it is
detached from the Emacs process."
  ;; Copied from `simple.el' and modified to call
  ;; `preamble-shell-command-silently' instead of
  ;; `shell-command'.
  (interactive
   (list
    (read-shell-command "Async shell command: " nil nil
                        (and buffer-file-name
                             (file-relative-name buffer-file-name)))))
  (unless (string-match "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (preamble-shell-command-silently command))

(global-set-key "\M-&" 'preamble-async-shell-command)

(defun preamble-xdg-open (filename)
  "Open file FILENAME in user's preferred application."
  (interactive
   (list (ido-read-file-name "Open file: ")))
  (preamble-async-shell-command
   (format "xdg-open \"%s\"" filename)))

(defun preamble-kill-autoload-buffers ()
  "Kill `-autoload.el' buffers loaded by `package'."
  (dolist (buffer (buffer-list))
    (when (string-match "-autoloads\\.el\\'" (buffer-name buffer))
      (kill-buffer buffer))))

(provide 'preamble-utils)
