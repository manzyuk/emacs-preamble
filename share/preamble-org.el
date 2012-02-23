(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-window-setup 'current-window)

(setq org-agenda-files '("~/org/todo.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-capture-templates
      '(("t" "TODO" entry (file "~/org/todo.org") "* TODO %?")))

(defun preamble-org-mode-in-block-p ()
  (let ((pos (max (1- (point)) (point-min))))
    (some (lambda (ovl)
            (eql (overlay-get ovl 'face)
                 'org-block-background))
          (overlays-at pos))))

(defun preamble-org-mode-in-block-delimiter-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^\s*#\\+\\(begin\\|end\\)_.*$")))

(defadvice org-mode-flyspell-verify (around org-mode-flyspell-verify-around)
  (setq ad-return-value
        (and ad-do-it
             (not (preamble-org-mode-in-block-p))
             (not (preamble-org-mode-in-block-delimiter-p)))))

(ad-activate 'org-mode-flyspell-verify)

(setq org-src-fontify-natively t)

(provide 'preamble-org)
