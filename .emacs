(setenv "PAGER"  "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")

;;; Startup

(server-start)

;; Don't display the startup screen.
(setq inhibit-startup-screen t)

;; Don't display any message in *scratch* buffer at startup.
(setq initial-scratch-message nil)

;;; Appearance

;; Disable display of a menu bar, tool bar, and scroll bars on all
;; frames, and disable cursor blinking.
(dolist (mode '(menu-bar-mode
                tool-bar-mode
                scroll-bar-mode
                blink-cursor-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; For buffers visiting files show the full file name in the title
;; bar; for buffers not associated with files show the buffer name.
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Visually indicate empty lines after the buffer end.
(setq-default indicate-empty-lines t)

;; Only show a cursor in the selected window.
(setq-default cursor-in-non-selected-windows nil)

;; Disable the 3D highlighting of the mode line.
(set-face-attribute 'mode-line nil :box nil)

;; Display column number in the mode line.
(column-number-mode 1)

;;; Editing

;; Kill and copy text only if there is an active region (the default
;; behavior when C-w kills the region between the mark and the point
;; even if the region is not active is irritating).

(defun my-kill-region-if-mark-active ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (message "There is no active region.")))

(defun my-kill-ring-save-if-mark-active ()
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (message "There is no active region.")))

(global-set-key (kbd "C-w")        'my-kill-region-if-mark-active)
(global-set-key (kbd "S-<delete>") 'my-kill-region-if-mark-active)
(global-set-key (kbd "M-w")        'my-kill-ring-save-if-mark-active)
(global-set-key (kbd "C-<insert>") 'my-kill-ring-save-if-mark-active)

;; When I select something in another program to paste it into Emacs,
;; but kill something in Emacs before actually pasting it, don't lose
;; the selection; save it in the `kill-ring' before the Emacs kill so
;; that I can still paste it using C-y M-y.
(setq save-interprogram-paste-before-kill t)

;; Let typed text replace the selection if the selection is active.
(delete-selection-mode 1)

;; Always add a new line at the end of the file.
(setq require-final-newline 'visit-save)

;; Visualize matching parens.
(show-paren-mode 1)

;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

;; Make mouse middle-click only paste from primary X11 selection, not
;; from clipboard or `kill-ring'.
(global-set-key [mouse-2] 'mouse-yank-primary)

;; Yank at point instead of at click.
(setq mouse-yank-at-point t)

;; Scroll commands move point to always keep its screen position
;; unchanged.
(setq scroll-preserve-screen-position 1)

;;; Files

;; Automatically save place in each file.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; Revert any buffer associated with a file when the file changes on
;; disk.
(global-auto-revert-mode 1)

;; Don't generate any messages whenever a file is reverted.
(setq auto-revert-verbose nil)

;; Revert also some non-file buffers.
(setq global-auto-revert-non-file-buffers t)

;; Place all backups into ~/.emacs.d/backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Uniquify buffer names with parts of directory name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Don't request confirmation before visiting a new file or buffer.
(setq confirm-nonexistent-file-or-buffer nil)

;; Clean up whitespace in the buffer on save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Automatically make scripts starting with #! executable.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Set the default coding system to UTF-8.
(prefer-coding-system 'utf-8-unix)

;;; Minibuffer

;; Save minibuffer history.
(savehist-mode 1)

;; Enable incremental minibuffer completion.
(icomplete-mode 1)

;; Use y/n instead of yes/no in confirmation dialogs.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Comint

(require 'comint)

;; Don't add input matching the last on the input ring.
(setq comint-input-ignoredups t)

;; Make the comint prompt read-only.
(setq comint-prompt-read-only t)

;; Interpreter output moves point to the end of the output, but only
;; in the selected window so that we can simultaneously look at
;; previous outputs in other windows.
(setq-default comint-move-point-for-output 'this)

;; Input to interpreter causes only the selected window to scroll.
(setq-default comint-scroll-to-bottom-on-input 'this)

;; Enable reading/writing of comint input ring from/to a history file.

(defun my-comint-write-history-on-exit (process event)
  (with-demoted-errors
    (comint-write-input-ring)
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (insert (format "\nProcess %s %s" process event)))))))

(defun my-turn-on-comint-history ()
  "Enable reading/writing of comint input ring from/to a history file."
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (concat user-emacs-directory
                    (format "inferior-%s-history" (process-name process))))
      (with-demoted-errors
        (comint-read-input-ring t))
      (set-process-sentinel process #'my-comint-write-history-on-exit))))

;; If the buffer associated with a process is killed, the process's
;; sentinel is invoked when buffer-local variables  (in particular,
;; `comint-input-ring-file-name' and `comint-input-ring') are gone.
;; Therefore try to save the history every time a buffer is killed.

(defun my-comint-write-input-ring-demoting-errors ()
  (with-demoted-errors
    (comint-write-input-ring)))

(add-hook 'kill-buffer-hook 'my-comint-write-input-ring-demoting-errors)

;; Apparently, when Emacs is killed, `kill-buffer-hook' is not run
;; on individual buffers.  We circumvent that by adding a hook to
;; `kill-emacs-hook' that walks the list of all buffers and writes
;; the input ring (if it is available) of each buffer to a file.

(defun my-comint-write-input-ring-all-buffers ()
  "Walk the list of all buffers and write the input ring (if it is
available) of each buffer to a history file."
  (with-demoted-errors
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (comint-write-input-ring)))))

(add-hook 'kill-emacs-hook 'my-comint-write-input-ring-all-buffers)

;;; Dired

;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)

;; Teach dired to uncompress zip files.
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

(require 'cl)

;; Copy the function definition of the symbol `shell-command', so that
;; we can dynamically shadow it with the help of `flet' and use the
;; old definition in the body of the new one without entering a loop.
(fset 'my-shell-command (symbol-function 'shell-command))

(defun my-shell-command-silently (command)
  "Execute string COMMAND in a subshell of inferior shell.  Don't
display any output or errors."
  (my-shell-command (format "( %s ) > /dev/null 2>&1" command)))

(defmacro silently (command)
  (let ((name (intern (concat (symbol-name command) "-silently"))))
    `(defadvice ,command (around ,name activate)
       (flet ((shell-command (command &optional output-buffer error-buffer)
                (my-shell-command-silently command))
              (message (format-string &rest args)
                nil))
         ad-do-it))))

(silently async-shell-command)
(silently dired-do-async-shell-command)

(defun my-dired-do-xdg-open ()
  "In Dired, open file mentioned on this line in user's preferred
application."
  (interactive)
  (dired-do-async-shell-command
   "xdg-open"
   nil
   (list (dired-get-filename t nil))))

(defun my-dired-hook ()
  (define-key dired-mode-map (kbd "RET")
    'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")
    (lambda ()
      (interactive)
      (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "V")
    'my-dired-do-xdg-open)
  (define-key dired-mode-map (kbd "E")
    'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook 'my-dired-hook)

;;; Ediff

;; Do everything in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Split windows horizontally rather than vertically.
(setq ediff-split-window-function 'split-window-horizontally)

;;; Hippie-expand

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(global-set-key (kbd "M-/") 'hippie-expand)

;;; Ibuffer

;; Replace the standard buffer menu with `ibuffer'.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Ido

;; Keep the home directory clean.
(setq ido-save-directory-list-file (concat user-emacs-directory "ido"))

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
      (goto-char (if (overlayp position) (overlay-start position) position))))
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

(global-set-key (kbd "C-c i") 'ido-goto-symbol)

;;; Makefile

(defun my-makefile-mode-hook ()
  ;; Don't use spaces instead of tabs.
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

;;; Shell

(require 'ansi-color)
(require 'shell)

;; Do not highlight any additional expressions in shell mode.
(setq shell-font-lock-keywords nil)

;; Track the current working directory by watching the prompt.
(defun my-turn-on-dirtrack-mode ()
  (dirtrack-mode 1))

(add-hook 'shell-mode-hook 'my-turn-on-dirtrack-mode)

;; Use bash when requesting inferior shells (also remote ones).
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)

(defun my-named-shell (name directory)
  "Open a named shell. NAME is the base name of the shell buffer,
and DIRECTORY is the directory to open the shell in."
  (interactive "MName: \nDDirectory: ")
  (switch-to-buffer (concat "*" name "*"))
  (cd directory)
  (shell (current-buffer)))

;;; Text

(defun my-text-mode-hook ()
  (turn-on-auto-fill)
  (turn-on-flyspell))

(add-hook 'text-mode-hook 'my-text-mode-hook)

;;; Package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(haskell-mode magit paredit)
  "List of packages to ensure are installed at launch.")

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;;; Paredit

(autoload 'enable-paredit-mode "paredit" nil t)

(eval-after-load "paredit"
  '(progn
     ;; Enable `paredit-mode' in the minibuffer during `eval-expression'.
     (defun my-turn-on-paredit-mode-in-minibuffer ()
       (when (eq this-command 'eval-expression)
         (enable-paredit-mode)))
     (add-hook 'minibuffer-setup-hook 'my-turn-on-paredit-mode-in-minibuffer)

     ;; Make C-<backspace> equivalent to M-DEL.
     (define-key paredit-mode-map
       (kbd "C-<backspace>") 'paredit-backward-kill-word)))

;;; Emacs Lisp

(defun my-emacs-lisp-mode-hook ()
  (enable-paredit-mode)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(defun my-ielm-mode-hook ()
  (enable-paredit-mode)
  (turn-on-eldoc-mode))

(add-hook 'ielm-mode-hook 'my-ielm-mode-hook)

;;; Haskell

(autoload 'haskell-mode "haskell-mode" nil t)

(setq haskell-indent-offset 2)

(defun my-haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (inf-haskell-mode 1)
  (subword-mode 1))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun my-inferior-haskell-mode-hook ()
  (my-turn-on-comint-history)
  (subword-mode 1))

(add-hook 'inferior-haskell-mode-hook 'my-inferior-haskell-mode-hook)

;;; Magit

(require 'magit)

(global-set-key (kbd "C-c g") 'magit-status)

(setq magit-last-seen-setup-instructions "1.4.0")

(set-face-attribute 'magit-item-highlight nil :inherit nil)

(unless (fboundp 'buffer-stale--default-function)
  (defun buffer-stale--default-function (&optional _noconfirm)
    (and buffer-file-name
         (file-readable-p buffer-file-name)
         (not (verify-visited-file-modtime (current-buffer))))))

;;; Org

;; Fontify code in code blocks.
(setq org-src-fontify-natively t)

(defun my-point-in-block-p ()
  (some (lambda (ovl)
          (eql (overlay-get ovl 'face)
               'org-block-background))
        (overlays-at (max (1- (point)) (point-min)))))

(defun my-point-in-block-delimiter-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^\s*#\\+\\(begin\\|end\\)_")))

(defadvice org-mode-flyspell-verify (around org-mode-flyspell-verify-around activate)
  "Don't let flyspell put overlays inside blocks and block delimiters."
  (setq ad-return-value
        (and ad-do-it
             (not (my-point-in-block-p))
             (not (my-point-in-block-delimiter-p)))))
