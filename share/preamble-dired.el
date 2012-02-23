;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)

;; Teach dired to uncompress zip files.
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(provide 'preamble-dired)
