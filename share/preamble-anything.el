(require 'preamble-utils)

(require 'anything-config)
(require 'anything-match-plugin)

(setq anything-mp-match-source-name nil)
(setq anything-mp-space-regexp "[\\][ ]")
(setq anything-candidate-number-limit nil)

(defun preamble-library ()
  "Interactively query the user for a pattern, narrow the list of
files from the directory given by the environment variable
LIBRARY down to those matching the pattern, and open the file the
user chooses in the preferred application.

The pattern is a space-separated list of strings, and the list is
narrowed down to filenames containing each of the strings."
  (interactive)
  (anything '(library-files)
            nil
            "View: "
            nil
            nil
            "*library*"))

(setq library-files
      `((name       . ,(getenv "LIBRARY"))
        (candidates . ,(preamble-shell-command-to-list "ls $LIBRARY"))
        (action     . (("Open in the preferred application"
                        .
                        (lambda (name)
                          (preamble-xdg-open
                           (format "$LIBRARY/%s" name))))))))

(global-set-key "\C-cv" 'preamble-library)

(provide 'preamble-anything)
