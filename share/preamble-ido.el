;; Keep the home directory clean.
(setq ido-save-directory-list-file
      (convert-standard-filename
       (concat user-emacs-directory "ido.last")))

;; Use flexible string matching with `ido-mode'.
(setq ido-enable-flex-matching t)

;; Always put . as the first item in file name lists; this allows the
;; current directory to be opened immediately with `dired'.
(setq ido-show-dot-for-dired t)

;; Show a file/buffer in selected window, even if that file/buffer is
;; visible in another frame.
(setq ido-default-file-method   'selected-window
      ido-default-buffer-method 'selected-window)

;; Enable `ido-mode'.
(ido-mode 1)

;; Enable `ido-everywhere'.
(ido-everywhere 1)

;; When I am opening a file (with `find-file' or `ido-find-file'), do
;; not suggest files generated by various programs.
(setq completion-ignored-extensions
      (nconc (list
              ;; Files generated by GHC
              ".hi"
              ;; Files generated by MIT Scheme
              ".bci" ".bin" ".com"
              ;; Files generated by LaTeX and friends
              ".dvi" ".pdf" ".ps"
              )
             completion-ignored-extensions))

;; Don't automatically merge work directories during file name input.
;; Use `M-s' to force the merge when it appears appropriate.
(setq ido-auto-merge-work-directories-length -1)

;; Taken from http://emacswiki.org/emacs/ImenuMode (see "Using Ido").
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh Imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol: " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key "\C-ci" 'ido-goto-symbol)

(provide 'preamble-ido)
