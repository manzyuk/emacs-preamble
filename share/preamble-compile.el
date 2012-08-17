;; Allow faces specified by property lists of the form (face FACE
;; PROP1 VAL1 PROP2 VAL2 ...) in `compilation-error-regexp-alist'.

(eval-after-load "compile"
  '(defun compilation-parse-errors (start end &rest rules)
     "Parse errors between START and END.
The errors recognized are the ones specified in RULES which default
to `compilation-error-regexp-alist' if RULES is nil."
     (dolist (item (or rules compilation-error-regexp-alist))
       (if (symbolp item)
           (setq item (cdr (assq item
                                 compilation-error-regexp-alist-alist))))
       (let ((file (nth 1 item))
             (line (nth 2 item))
             (col (nth 3 item))
             (type (nth 4 item))
             (pat (car item))
             end-line end-col fmt
             props)

         ;; omake reports some error indented, so skip the indentation.
         ;; another solution is to modify (some?) regexps in
         ;; `compilation-error-regexp-alist'.
         ;; note that omake usage is not limited to ocaml and C (for stubs).
         ;; FIXME-omake: Doing it here seems wrong, at least it should depend on
         ;; whether or not omake's own error messages are recognized.
         (cond
          ((not (memq 'omake compilation-error-regexp-alist)) nil)
          ((string-match "\\`\\([^^]\\|^\\( \\*\\|\\[\\)\\)" pat)
           nil) ;; Not anchored or anchored but already allows empty spaces.
          (t (setq pat (concat "^ *" (substring pat 1)))))

         (if (consp file)	(setq fmt (cdr file)      file (car file)))
         (if (consp line)	(setq end-line (cdr line) line (car line)))
         (if (consp col)	(setq end-col (cdr col)   col (car col)))

         (if (functionp line)
             ;; The old compile.el had here an undocumented hook that
             ;; allowed `line' to be a function that computed the actual
             ;; error location.  Let's do our best.
             (progn
               (goto-char start)
               (while (re-search-forward pat end t)
                 (save-match-data
                   (when compilation-debug
                     (font-lock-append-text-property
                      (match-beginning 0) (match-end 0)
                      'compilation-debug (vector 'functionp item)))
                   (add-text-properties
                    (match-beginning 0) (match-end 0)
                    (compilation--compat-error-properties
                     (funcall line (cons (match-string file)
                                         (cons default-directory
                                               (nthcdr 4 item)))
                              (if col (match-string col))))))
                 (compilation--put-prop
                  file 'font-lock-face compilation-error-face)))

           (unless (or (null (nth 5 item)) (integerp (nth 5 item)))
             (error "HYPERLINK should be an integer: %s" (nth 5 item)))

           (goto-char start)
           (while (re-search-forward pat end t)
             (when (setq props (compilation-error-properties
                                file line end-line col end-col (or type 2) fmt))

               (when (integerp file)
                 (compilation--put-prop
                  file 'font-lock-face
                  (if (consp type)
                      (compilation-face type)
                    (symbol-value (aref [compilation-info-face
                                         compilation-warning-face
                                         compilation-error-face]
                                        (or type 2))))))

               (compilation--put-prop
                line 'font-lock-face compilation-line-face)
               (compilation--put-prop
                end-line 'font-lock-face compilation-line-face)

               (compilation--put-prop
                col 'font-lock-face compilation-column-face)
               (compilation--put-prop
                end-col 'font-lock-face compilation-column-face)

               (dolist (extra-item (nthcdr 6 item))
                 (let ((mn (pop extra-item)))
                   (when (match-beginning mn)
                     (let ((face (eval (car extra-item))))
                       (cond
                        ((null face))
                        ((symbolp face)
                         (put-text-property
                          (match-beginning mn) (match-end mn)
                          'font-lock-face face))
                        ((and (listp face)
                              (eq (car face) 'face))
                         (add-text-properties
                          (match-beginning mn) (match-end mn)
                          (cddr face)))
                        (t
                         (error "Don't know how to handle face %S"
                                face)))))))
               (let ((mn (or (nth 5 item) 0)))
                 (when compilation-debug
                   (font-lock-append-text-property
                    (match-beginning 0) (match-end 0)
                    'compilation-debug (vector 'std item props)))
                 (add-text-properties
                  (match-beginning mn) (match-end mn)
                  (cddr props))
                 (font-lock-append-text-property
                  (match-beginning mn) (match-end mn)
                  'font-lock-face (cadr props))))))))))

(provide 'preamble-compile)
