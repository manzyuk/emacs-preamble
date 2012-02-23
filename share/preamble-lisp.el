(require 'preamble-paredit)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; The following assumes that our Common Lisp implementation is
;; SBCL and SLIME has been installed via Quicklisp, for example
;; as described here:
;;  http://www.mohiji.org/2011/01/modern-common-lisp-on-linux.
;; If this is not the case, SLIME customizations are not loaded.
(let ((slime-helper-el (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper-el)
    (load slime-helper-el)

    (setq slime-lisp-implementations
          '((sbcl ("sbcl") :coding-system utf-8-unix)))

    (setq slime-autodoc-use-multiline-p t)

    (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)))

;; Enable symbol lookup in the HyperSpec via Info system.  The
;; following assumes that HyperSpec in Texinfo format has been
;; installed as described here:
;;          http://users-phys.au.dk/harder/dpans.html.

(require 'info-look)

(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(info-lookup-add-help
 :mode 'slime-repl-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(provide 'preamble-lisp)
