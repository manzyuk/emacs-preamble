(defun prelude-regexp-alternatives (regexps)
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

;; Make `async-shell-command' more useful: supress (i.e., redirect
;; to /dev/null) all output and errors and disown the process.
(defun preamble-async-shell-command (command)
  "Execute shell command COMMAND asynchronously in background.

Unlike `async-shell-command', suppress (i.e., redirect to /dev/null)
all output and errors and disown the process."
  (interactive
   (list
    (read-shell-command "Run: " nil nil
                        (and buffer-file-name
                             (file-relative-name buffer-file-name)))))
  (shell-command
   (concat command " > /dev/null 2>&1 & disown")))

(global-set-key "\M-&" 'preamble-async-shell-command)

(defun preamble-xdg-open (filename)
  "Open file FILENAME in the user's preferred application."
  (iteractive
   (list (ido-read-file-name "Open file: ")))
  (preamble-async-shell-command
   (format "xdg-open \"%s\"" filename)))

(provide 'preamble-utils)
