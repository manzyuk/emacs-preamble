(require 'google-translate)

(setq google-translate-default-source-language "en"
      google-translate-default-target-language "ru")

(setq google-translate-enable-ido-completion t)

(global-set-key "\C-ct" 'google-translate-query-translate)

(provide 'preamble-google-translate)
