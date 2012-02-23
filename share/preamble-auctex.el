;; Don't fontify subscript and superscript strings.
(setq font-latex-fontify-script nil)

;; Fotify sectioning macros with a color face only.
(setq font-latex-fontify-sectioning 'color)

;; Automatically save style information when saving the buffer.
(setq TeX-auto-save t)

;; Parse file after loading it if no style hook is found for it.
(setq TeX-parse-self t)

;; Don't ask for confirmation to save files before starting TeX.
(setq TeX-save-query nil)

;; Use source specials for forward and inverse search.
(setq TeX-source-correlate-method 'source-specials)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-source-correlate-mode 1)))

;; Start server for inverse search.
(setq TeX-source-correlate-start-server t)

(require 'reftex)

;; Turn RefTeX plug-ins on in LaTeX buffers.
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(provide 'preamble-auctex)
