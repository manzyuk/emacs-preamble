(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(global-set-key "\M-/" 'hippie-expand)

(provide 'preamble-hippie-expand)
