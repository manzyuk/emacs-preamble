(require 'ansi-color)

;; Define colors a la the default gnome-terminal color theme.

(setq preamble-color0  "#000000"
      preamble-color1  "#CC0000"
      preamble-color2  "#4E9A06"
      preamble-color3  "#C4A000"
      preamble-color4  "#3465A4"
      preamble-color5  "#75507B"
      preamble-color6  "#06989A"
      preamble-color7  "#D3D7CF"
      preamble-color8  "#555753"
      preamble-color9  "#ef2929"
      preamble-color10 "#8ae234"
      preamble-color11 "#fce94f"
      preamble-color12 "#729fcf"
      preamble-color13 "#ad7fa8"
      preamble-color14 "#34e2e2"
      preamble-color15 "#eeeeec")

(setq preamble-black        preamble-color0
      preamble-bold-black   preamble-color8
      preamble-red          preamble-color1
      preamble-bold-red     preamble-color9
      preamble-green        preamble-color2
      preamble-bold-green   preamble-color10
      preamble-yellow       preamble-color3
      preamble-bold-yellow  preamble-color11
      preamble-blue         preamble-color4
      preamble-bold-blue    preamble-color12
      preamble-magenta      preamble-color5
      preamble-bold-magenta preamble-color13
      preamble-cyan         preamble-color6
      preamble-bold-cyan    preamble-color14
      preamble-white        preamble-color7
      preamble-bold-white   preamble-color15)

(defvar preamble-bold-colors
  `((,preamble-black   . ,preamble-bold-black  )
    (,preamble-red     . ,preamble-bold-red    )
    (,preamble-green   . ,preamble-bold-green  )
    (,preamble-yellow  . ,preamble-bold-yellow )
    (,preamble-blue    . ,preamble-bold-blue   )
    (,preamble-magenta . ,preamble-bold-magenta)
    (,preamble-cyan    . ,preamble-bold-cyan   )
    (,preamble-white   . ,preamble-bold-white  ))
  "List of colors used to display normal and bold text.

Each element is a cons-cell of the form (NORMAL-COLOR . BOLD-COLOR),
where NORMAL-COLOR is the color used for displaying normal text, and
BOLD-COLOR is its slightly brighter version used for displaying bold
text.")

(defun preamble-boldify-color (color)
  "Return a bold version of COLOR.  If COLOR doesn't have a brighter
bold version, return COLOR."
  (or (cdr (assoc color preamble-bold-colors))
      color))

(defun preamble-boldify-face (face)
  "Boldify the color slot of FACE.  If FACE has no color associated
with it, return FACE."
  (if (consp face)
      (let* ((property   (car face))
             (color      (cdr face))
             (bold-color (preamble-boldify-color color)))
        (ansi-color-make-face property bold-color))
    face))

(eval-after-load "ansi-color"
  '(progn
     ;; Copied from `ansi-color.el' and modified to display bold faces
     ;; using slighly different, brigher colors.
     (defun ansi-color-get-face (escape-seq)
       (let ((i 0)
             f val)
         (while (string-match ansi-color-parameter-regexp escape-seq i)
           (setq i (match-end 0)
                 val (ansi-color-get-face-1
                      (string-to-number (match-string 1 escape-seq) 10)))
           (cond ((not val))
                 ((eq val 'default)
                  (setq f (list val)))
                 (t
                  (unless (member val f)
                    (push val f)))))
         ;; Use brighter colors for bold faces.
         (when (member 'bold f)
           (setq f (mapcar 'preamble-boldify-face f)))
         f))
     ;; Copied from `ansi-color.el' and modified to support so called
     ;; high intensity colors.
     (defun ansi-color-make-color-map ()
       (let ((ansi-color-map (make-vector 110 nil))
             (index 0))
         ;; miscellaneous attributes
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index e)
                      (setq index (1+ index)) ))
          ansi-color-faces-vector)
         ;; foreground attributes
         (setq index 30)
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index
                            (ansi-color-make-face 'foreground e))
                      (setq index (1+ index)) ))
          ansi-color-names-vector)
         ;; background attributes
         (setq index 40)
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index
                            (ansi-color-make-face 'background e))
                      (setq index (1+ index)) ))
          ansi-color-names-vector)
         ;; foreground attributes -- high intensity
         (setq index 90)
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index
                            (ansi-color-make-face 'foreground e))
                      (setq index (1+ index)) ))
          ansi-color-names-vector)
         ;; background attributes -- high intensity
         (setq index 100)
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index
                            (ansi-color-make-face 'background e))
                      (setq index (1+ index)) ))
          ansi-color-names-vector)
         ansi-color-map))))

(setq ansi-color-names-vector
      (vector preamble-black
              preamble-red
              preamble-green
              preamble-yellow
              preamble-blue
              preamble-magenta
              preamble-cyan
              preamble-white))

(provide 'preamble-ansi-color)
