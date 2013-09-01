(add-to-list 'load-path "~/.opam/4.01.0dev+trunk/share/emacs/site-lisp")

(autoload 'utop "utop" "Toplevel for OCaml" t)

(setq utop-command "opam config exec \"utop -emacs\"")

(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))

(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (define-key tuareg-mode-map "\C-c\C-s" 'tuareg-run-caml)))

(provide 'preamble-ocaml)
