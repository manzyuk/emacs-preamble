;; Always automatically save my place in any file.
(setq save-place-file
      (convert-standard-filename
       (concat user-emacs-directory "places")))
(setq-default save-place t)
(require 'saveplace)

;; Don't ask to select recipients when encrypting files.  Instead use
;; `epa-file-encrypt-to' local variable to specify those.
(setq epa-file-select-keys nil)

(provide 'preamble-data)
