(require 'google-translate)

(setq google-translate-default-source-language "en"
      google-translate-default-target-language "ru")

(setq google-translate-enable-ido-completion t)

(global-set-key (kbd "C-c t")
                'google-translate-at-point)
(global-set-key (kbd "C-c T")
                'google-translate-query-translate)
(global-set-key (kbd "C-c r")
                'google-translate-at-point-reverse)
(global-set-key (kbd "C-c R")
                'google-translate-query-translate-reverse)

(provide 'preamble-google-translate)
