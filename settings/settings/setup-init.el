;;; Key bindings

;;;; remapping

(define-key key-translation-map (kbd "M-q") (kbd "C-g"))
(define-key key-translation-map (kbd "C-<return>") (kbd "C-c"))

;;;; hydra

(require 'hydra)

(setq-default hydra-hint-display-type  'message)

;;;; key-chord

(require 'key-chord)

(key-chord-mode t)
(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.2)

;;;; to sort

(global-set-key (kbd "C-c q") 'counsel-quick-access)
(global-set-key (kbd "C-a") 'counsel-find-file)

(global-set-key (kbd "C-x C-e") 'ta-fzf-emacs-settings)
(global-set-key (kbd "<up>") 'ta-fzf-dwim)

(global-set-key (kbd "<f5>") 'ta-find-file-notes)
(key-chord-define-global ";;" 'counsel-M-x)
(global-set-key (kbd "C->") 'delete-char)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
(global-set-key (kbd "M-l") 'recenter-top-bottom)
(global-set-key (kbd "C-v") 'visual-line-mode)
(global-set-key (kbd "<f3>") 'yank)
(global-set-key (kbd "<C-escape>") 'repeat)

(global-set-key (kbd "C-s") 'linux-switch-keyboard-layout)
(global-set-key (kbd "C-c f") 'ta-copy-buffer-file-name)

(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "M-+") 'undo-redo)

;;;; linux and other commands with transient interface

(require 'linux)
(require 'transient)
(require 'yasnippet)

(defmacro ta-transient-define-suffix (command)
  "Create a command COMMAND--transient that is a transient suffix command
that call interactively COMMAND."
  (let ((func-name (concat (symbol-name command) "--transient")))
    `(transient-define-suffix ,(intern func-name) ()
       (interactive)
       (call-interactively (quote ,command)))))

(ta-transient-define-suffix describe-key)
(ta-transient-define-suffix describe-keymap)
(ta-transient-define-suffix describe-function)
(ta-transient-define-suffix describe-variable)
(ta-transient-define-suffix describe-mode)
(ta-transient-define-suffix linux-switch-keyboard-layout)
(ta-transient-define-suffix linux-turn-off-laptop-output)
(ta-transient-define-suffix linux-toggle-dpi)
(ta-transient-define-suffix linux-toggle-git-commit-msg)
(ta-transient-define-suffix yas-describe-tables)
(ta-transient-define-suffix yas-new-snippet)
(ta-transient-define-suffix yas-visit-snippet-file)
(ta-transient-define-suffix apropos)
(ta-transient-define-suffix info)
(ta-transient-define-suffix image-toggle-display)
(ta-transient-define-suffix display-line-numbers-mode)

(transient-define-prefix ta-remind-me ()
  "Show menu buffer for miscellaneous commands I often need but do not remember."
  [["describe"
    ("dk" "describe-key" describe-key--transient)
    ("dp" "describe-keymap" describe-keymap--transient)
    ("df" "describe-function" describe-function--transient)
    ("dv" "describe-variable" describe-variable--transient)
    ("dm" "describe-mode" describe-mode--transient)]
   ["linux"
    ("lk" "linux-switch-keyboard-layout" linux-switch-keyboard-layout--transient)
    ("ll" "linux-turn-off-laptop-output" linux-turn-off-laptop-output--transient)
    ("ld" "linux-toggle-dpi" linux-toggle-dpi--transient)
    ("lg" "linux-toggle-git-commit-msg" linux-toggle-git-commit-msg--transient)]
   ["yasnippet"
    ("sd" "yas-describe-tables" yas-describe-tables--transient)
    ("sn" "yas-new-snippet" yas-new-snippet--transient)
    ("sf" "yas-visit-snippet-file" yas-visit-snippet-file--transient)]
   ["misc"
    ("a" "apropos" apropos--transient)
    ("i" "info" info--transient)
    ("t" "image-toggle-display" image-toggle-display--transient)
    ("n" "display-line-numbers-mode" display-line-numbers-mode--transient)]])

(global-set-key (kbd "C-M-i") 'ta-remind-me)

;;; Global

;;;; Appearance

;;;;; Cursor

(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 2))


;;;;; Font

(set-face-attribute 'default nil :family "DejaVu Sans Mono")
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;;;;; Window

(setq-default fringe-indicator-alist
              (push '(truncation nil nil) fringe-indicator-alist))

;;;;; Frame

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
(setq frame-resize-pixelwise t)

;;;;; Lines

(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)

;;;;; Mode line
;;;;;; Packages

(require 'moody)
(require 'minions)
(require 'keycast)

;;;;;; Utility function

(defun ta-number-of-lines-mode ()
  "Return the number of lines in the whole buffer."
  (interactive)
  (int-to-string (count-lines (point-min) (point-max))))

;;;;;; mode-line-format

(column-number-mode t)
(line-number-mode t)

(setq x-underline-at-descent-line t)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                moody-mode-line-buffer-identification
                "   "
                (:eval (format "NL%s" (ta-number-of-lines-mode)))
                " "
                mode-line-position
                (vc-mode moody-vc-mode)
                "  "
                minions-mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;;;;;; minions

(setq minions-direct '(insight-mode))
(setq minions-mode-line-lighter "")
(setq minions-mode-line-delimiters '("" . ""))

;;;;;; keycast

(setq keycast-separator-width 2)
(setq keycast-insert-after 'moody-mode-line-buffer-identification)
(setq keycast-window-predicate 'moody-window-active-p)
(setq keycast-remove-tail-elements nil)

(setq keycast-substitute-alist
      '((self-insert-command "." "self-insert-command")
        (org-self-insert-command "." "org-self-insert-command")
        (outline-spc-self-insert-command "." "outline-spc-self-insert-command")))

;;;;;; moody

(setq moody-mode-line-height 30)

(defun set-moody-face (frame)
  (let ((line (face-attribute 'mode-line :underline frame)))
    (set-face-attribute 'mode-line          frame :overline   line)
    (set-face-attribute 'mode-line-inactive frame :overline   line)
    (set-face-attribute 'mode-line-inactive frame :underline  line)
    (set-face-attribute 'mode-line          frame :box        nil)
    (set-face-attribute 'mode-line-inactive frame :box        nil)))

(add-to-list 'after-make-frame-functions 'set-moody-face t)

;;;; Buffers

(require 'ivy)

(set-language-environment "UTF-8")
(setq save-interprogram-paste-before-kill t)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting.

see: http://github.com/magnars"
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun ta-clone-indirect-buffer (narrow)
  "Create an indirect buffer with name composed with NARROW string.

NARROW, a string, is the name of the section/function you are narrowing
in the indirect buffer.  The name of the indirect buffer is composed
with the `buffer-name' and NARROW.

The indirect buffer is displayed in the selected window.

See `clone-indirect-buffer'."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
         (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (read-string "Narrowed part name: "))))
  (let* ((newname (format "%s::%s" (buffer-name) narrow))
         (name (generate-new-buffer-name newname))
         (buffer (make-indirect-buffer (current-buffer) name t)))
    (switch-to-buffer buffer)
    buffer))

(global-set-key (kbd "C-c c") 'ta-clone-indirect-buffer)
(global-set-key [escape] 'kill-this-buffer)
(global-set-key (kbd "<f6>") 'save-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<right>") 'next-buffer)
(global-set-key (kbd "<down>") 'ivy-switch-buffer-other-window)
(global-set-key (kbd "C-e") 'ivy-switch-buffer)

;;;; Enable commands

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;;; Indentation

(require 'whitespace)

(setq fill-column 72)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq whitespace-style '(face tabs tab-mark lines-tail))
(setq whitespace-line-column nil)
(setq whitespace-display-mappings '((tab-mark ?\t [?⇥ ?\ ])))

(add-hook 'prog-mode-hook 'whitespace-mode)

;;;;; Comments

(comment ; info on tabs vs. spaces
 (info "(emacs) Just Spaces"))

(comment
 (info "(elisp) Character Codes")
 ;; https://en.wikipedia.org/wiki/Tab_key
 ;; U+21E5 ⇥ RIGHTWARDS ARROW TO BAR
 (char-from-name "RIGHTWARDS ARROW TO BAR") ; 8677
 )

;;;; Windows

(require 'hydra)
(require 'framer)
(require 'transpose-frame)

(winner-mode t)

(setq-default
 display-buffer-alist
 '((".*occur.*\\|\\*grep.*\\|\\*rg.*\\|*Occur.*"
    (display-buffer-same-window))
   ("\\*Help.*\\|\\*Messages.*\\|\\*Warnings.*\\|\\*Backtrace.*"
    (display-buffer-same-window))
   ("\\*YASnippet Tables by NAMEHASH\\*\\|\\*YASnippet Tables\\*"
    (display-buffer-same-window))
   ("\\*info.*\\|\\*Apropos.*"
    (display-buffer-same-window))
   ("\\*Pp Eval Output.*\\|\\*Org Src .*"
    (display-buffer-same-window))
   ("magit:.*"
    (display-buffer-same-window))
   ("magit-log:.*"
    (display-buffer-same-window))))

(defun ta-swap-windows ()
  "Swap buffers of `selected-window' and `next-window'."
  (interactive)
  (let ((buffer1 (current-buffer))
        (buffer2 (window-buffer (next-window)))
        (win (next-window)))
    (set-window-buffer (selected-window) buffer2)
    (set-window-buffer (next-window) buffer1)
    (select-window win)))

(defhydra hydra-windows
  (:pre (progn
          (remove-hook 'post-command-hook 'insight-check-cursor-color)
          (set-cursor-color "#ffd500"))
   :post (progn
           (add-hook 'post-command-hook 'insight-check-cursor-color)
           (set-cursor-color "#26f9ad"))
   :hint nil)
  ("t" handy-line/body :color blue)
  ("M-t" transpose-frame)
  ("M-s" ta-swap-windows)
  ("u" winner-undo)
  ("]" winner-redo)
  ("." framer-push :color blue)
  ("x" framer-undo)
  (":" framer-redo)
  ("q" nil))

(global-set-key (kbd "C-o") 'delete-other-windows)
(global-set-key (kbd "M-o") 'delete-window)
(global-set-key (kbd "M-u") 'hydra-windows/body)
(global-set-key (kbd "M->") 'other-window)

;;;; PATH

(setenv "PATH" (concat "~/.local/bin:~/.linuxbrew/bin:"
                       (getenv "PATH")))
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/.linuxbrew/bin/")

;;;; Other

(defun ta-delete-trailing-whitespace ()
  ;; Don't delete trailing whitespace in PDFs to avoid
  ;; corrupting them.
  (let ((extension (file-name-extension buffer-file-name)))
    (unless (and extension (string= "pdf" (downcase extension)))
      (delete-trailing-whitespace))))

(add-hook 'before-save-hook 'ta-delete-trailing-whitespace)

(setq create-lockfiles nil)

(setq browse-url-browser-function 'browse-url-chromium)
(setq backup-directory-alist '(("." . "~/.emacs.d/generated/backup")))
(add-to-list `auto-mode-alist '("\\.svg\\'" . nxml-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'recentf)

(save-place-mode t)
(global-auto-revert-mode 1)
(recentf-mode 1)

(setq save-place-file "~/.emacs.d/generated/places")
(setq recentf-save-file "~/.emacs.d/generated/recentf")
(setq recentf-max-saved-items 25)

(setq scroll-preserve-screen-position 1) ; TODO: maybe add a command to switch between `t' and `1' in `insight-mode'
(setq scroll-conservatively 100)
(setq recenter-positions '(middle top bottom))

(setq custom-file "~/.emacs.d/generated/custom.el")
(load custom-file)

(setq comment-empty-lines t)
(setq comment-padding 1)

(setq-default truncate-lines t)

;;; Per Mode

;;;; css-mode and sgml-mode

(setq css-indent-offset 2)

(require 'emmet-mode)
(require 'sgml-mode)

(define-key sgml-mode-map (kbd "C-M-i") nil)

(define-key sgml-mode-map (kbd "C-<tab>") 'emmet-expand-line)
(define-key sgml-mode-map (kbd "<f1>") 'ta-previous-attribute)
(define-key sgml-mode-map (kbd "<f2>") 'ta-next-attribute)

(add-hook 'css-mode-hook (lambda ()
                           (company-mode 1)
                           (setq company-minimum-prefix-length 1)))

;;;; csv-mode

;; if you want to use tab separator, use `tsv-mode' derived from `csv-mode',
;; because only settings `csv-separators' to '("\t") won't work.
(setq csv-separators '(","))

(define-minor-mode header-line-csv-mode
  "Set the `header-line-format' to be the first line of the current buffer.

This is handy in `csv-mode' when you scroll and want to know what are the header
of the columns."
  :global nil
  (let* ((beg (progn (beginning-of-buffer) (point)))
         (end (progn (end-of-line) (point)))
         (header-string (buffer-substring-no-properties beg end)))
    (setq header-line-format header-string)))



;;;; clojure-mode

(require 'clojure-mode)
(require 'outline)
(require 'outline-spc)

(defun ta-clojure-mode-outline ()
  "Hook to turn on `outline-minor-mode'."
  (outline-minor-mode t)
  (outline-spc-mode t)
  (setq-local outline-regexp ";;;;* \\|("))

(add-hook 'clojure-mode-hook #'ta-clojure-mode-outline)

(define-key clojure-mode-map (kbd "TAB") 'bicycle-cycle)

;;;; company-mode

(require 'company)

(setq company-selection-wrap-around t)
(setq company-tooltip-limit 10)
(setq company-require-match nil)
(setq company-idle-delay 0)
(setq company-backends '(company-capf company-files))
(setq company-format-margin-function nil)
(make-variable-buffer-local 'company-idle-delay)
(make-variable-buffer-local 'company-minimum-prefix-length)
(make-variable-buffer-local 'company-backends)

(define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
(define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)
(define-key company-active-map (kbd "<prior>") 'company-previous-page)
(define-key company-active-map (kbd "<next>") 'company-next-page)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)
(define-key company-active-map (kbd "M-t") 'company-filter-candidates)
(define-key company-active-map (kbd "M-q") 'company-abort)

;;;; compilation-mode and next-error

(require 'compile)
(require 'simple)

(defun ta-next-error-no-select (&optional n)
  "Similar to `next-error-no-select' but highlight match in a right window."
  (interactive "p")
  (save-selected-window
    (let ((next-error-highlight next-error-highlight-no-select)
          (display-buffer-overriding-action
           (if (eq 2 (length (window-list)))
               '(nil (inhibit-same-window . t))
             '(display-buffer-in-direction (direction . right)
                                           (window-width . 0.74)))))
      (next-error n))))

(defun ta-previous-error-no-select (&optional n)
  "Similar to `previous-error-no-select' but highlight match in a right window."
  (interactive "p")
  (ta-next-error-no-select (- (or n 1))))

(defalias 'next-error-no-select 'ta-next-error-no-select)
(defalias 'previous-error-no-select 'ta-previous-error-no-select)

;;;; emacs-lisp-mode

(require 'outline)
(require 'outline-spc)
(require 'aggressive-indent)

(setq lisp-indent-function 'fuco-lisp-indent-function)

(defun ta-emacs-lisp-mode-outline ()
  "Hook to turn on `outline-minor-mode'."
  (outline-minor-mode t)
  (outline-spc-mode t))

(defun ta-emacs-lisp-mode-company ()
  "Setup `company-mode' for `emacs-lisp-mode-hook'"
  (company-mode 1)
  (setq company-backends '((company-capf company-files)))
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 1))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'ta-emacs-lisp-mode-outline)
(add-hook 'emacs-lisp-mode-hook #'ta-emacs-lisp-mode-company)

(define-key emacs-lisp-mode-map (kbd "C-M-i") nil)
(define-key emacs-lisp-mode-map (kbd "TAB") 'bicycle-cycle)

;;;;; indent function (from Fuco)

(defun fuco-lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation.

Also redefines the silly indent of keyword lists:
before
  (:foo bar
        :baz qux)
after
  (:foo bar
   :baz qux)

see: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94"
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

;;;; emmet-mode

(require 'emmet-mode)

(setq emmet-indentation 4)
(setq emmet-indent-after-insert t)
(setq emmet-use-style-tag-and-attr-detection t)
(setq emmet-self-closing-tag-style "/")
(setq emmet-insert-flash-time 0.001)
(setq emmet-move-cursor-after-expanding t)
(setq emmet-move-cursor-between-quotes t)
(setq emmet-postwrap-goto-edit-point t)

(defun ta-emmet-css-mode-hook ()
  "Emmet with css-transform"
  (emmet-mode t)
  (setq emmet-use-css-transform t))

(add-hook 'css-mode-hook 'ta-emmet-css-mode-hook)
(add-hook 'sgml-mode-hook  'emmet-mode)
(add-hook 'js-mode-hook  'emmet-mode)

;;;; fzf.el

(require 'fzf)

(setq fzf/directory-start "~/work/")

(setq fzf/window-height 15) ; default
(setq fzf/executable "/home/tony/.linuxbrew/bin/fzf")
(setq fzf/git-grep-args "-i --line-number %s") ; default
(setq fzf/position-bottom nil) ; default

;; fzf/args
;; 1. don't use "--reverse" argument, the way fzf/after-term-handle-exit
;;    handle the parsing to retrieve the selected file doesn't cover this case.
;; 2. "--print-query" is a mandatory argument, if you remove it, fzf/after-term-handle-exit
;;    doesn't do well its job.
(setq ta-fzf/bind
      (concat " --bind "
              "'"
              "ctrl-s:reload(fd),"                               ;; in ctrl-s 's' stands for Standard
              "ctrl-h:reload(fd --hidden --exclude \".git/*\")," ;; in ctrl-h 'h' stands for Hidden
              "ctrl-n:reload(fd --hidden --no-ignore),"          ;; in ctrl-n 'n' stands for No-ignore
              "ctrl-d:reload(fd --type d),"                      ;; in ctrl-d 'd' stands for Directory
              "ctrl-l:reload(fd --type d --hidden --no-ignore)," ;; in ctrl-l 'l' because can't take upcase 'D'
              "ctrl-f:reload(fd --type f),"                      ;; in ctrl-f 'f' stands for File
              "ctrl-b:reload(fd --type f --hidden --no-ignore)"  ;; in ctrl-b 'b' because can't take upcase 'F'
              "'"))
(setq fzf/args (concat "--print-query " ta-fzf/bind))

(defun ta-fzf-emacs-settings ()
  "fzf in my emacs settings directory."
  (interactive)
  (let ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=fd")
                                   process-environment)))
    (fzf/start "~/work/settings/emacs.d/.emacs.d/")))

(defun ta-fzf-directory ()
  "Start a fzf session frome `fzf/directory-start'."
  (interactive)
  (let ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=fd")
                                   process-environment)))
    (fzf/start fzf/directory-start)))

(defun ta-fzf-default-directory ()
  "Start a fzf session at the specified directory.

Select the start directory from `default-directory'."
  (interactive)
  (let ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=fd")
                                   process-environment)))
    (fzf/start (read-directory-name "Directory:" default-directory))))

(defun ta-fzf-project ()
  "Start a fzf session in current project.

If there is no current project, use the `default-directory'."
  (interactive)
  (let ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=fd")
                                   process-environment)))
    (if-let ((proj (cdr (project-current))))
        (fzf/start proj)
      (fzf/start default-directory))))

(defun ta-fzf-dwim (&optional arg)
  "Start a fzf session in current project.

If there is no current project, use the `default-directory'.

With \\[universal-argument] prefix, ask to choose a directory to start fzf.
With two \\[universal-argument] prefix, start fzf at from `fzf/directory-start'."
  (interactive "p")
  (cond
   ((equal arg 4) (call-interactively 'ta-fzf-default-directory))
   ((equal arg 16) (call-interactively 'ta-fzf-directory))
   (t (call-interactively 'ta-fzf-project))))

;;;; graphviz-dot-mode
(require 'graphviz-dot-mode)

(setq graphviz-dot-auto-preview-on-save t)
(setq graphviz-dot-dot-program "dot")

;;;; ibuffer-mode

(require 'ibuffer)

(defalias 'list-buffers 'ibuffer)

(setq ibuffer-expert t)
(setq ibuffer-use-header-line nil)
(setq ibuffer-formats
      '((mark
         modified " "
         (name 28 28 :left :elide) " "
         filename-and-process)))

(defun ta-ibuffer-preview ()
  "Preview buffer at `point'."
  (interactive)
  (when-let ((buffer-at-point (ibuffer-current-buffer t)))
    (if (eq 2 (length (window-list)))
        (display-buffer buffer-at-point t)
      (display-buffer buffer-at-point
                      '(display-buffer-in-direction
                        (direction . right)
                        (window-width . 0.74))))))

(defun ta-ibuffer-previous ()
  "Preview buffer on the previous line."
  (interactive)
  (ibuffer-backward-line)
  (ta-ibuffer-preview))

(defun ta-ibuffer-next ()
  "Preview buffer on the next line."
  (interactive)
  (ibuffer-forward-line)
  (ta-ibuffer-preview))

(define-key ibuffer-mode-map (kbd "M-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)
(define-key ibuffer-mode-map (kbd "p") 'ta-ibuffer-previous)
(define-key ibuffer-mode-map (kbd "n") 'ta-ibuffer-next)
(define-key ibuffer-mode-map (kbd "M-s") 'isearch-forward)
(key-chord-define-global "::" 'ibuffer)

;;;; insight-mode

(require 'insight)
(setq insight-cursor-color "#fd971f")
(insight-use-cursor-color)

(define-key insight-mode-map (kbd "t") 'handy-line/body)
(global-set-key (kbd "M-i") 'insight-mode)

;;;; js-mode
;;;;; Packages

(require 'outline)
(require 'outline-spc)
(require 'js)

(setq js-indent-level 2)

(defun ta-tide-mode-hook ()
  "Setup function for tide."
  (interactive)
  (tide-setup))

(defun ta-outline-js-mode-hook ()
  "Set up `outline-mode' and `bicycle'.  Specifically,

the variable `outline-regexp'."
  (outline-minor-mode t)
  (setq-local outline-regexp (concat
                              "//\\|"
                              "const\\|"
                              "class\\|"
                              "app\\|"
                              "fs\\|"
                              "/\\*\\*\\|"
                              "function\\|"
                              ;; testing with jest
                              "[[:space:]]*it\\|"
                              "describe\\|"
                              ;; React
                              "ReactDom")))

(add-hook 'js-mode-hook 'ta-outline-js-mode-hook)
(add-hook 'js-mode-hook #'ta-tide-mode-hook)
(add-hook 'js-mode-hook #'subword-mode)

;;;;; Utility functions

(defun ta-jsx-uncomment-line ()
  "Comment line of elements or components in `js-jsx-mode'."
  (back-to-indentation)
  (save-excursion
    (delete-char 3)
    (end-of-line)
    (delete-char -3)))

(defun ta-jsx-comment-line ()
  "Uncomment line of elements or components in `js-jsx-mode'."
  (back-to-indentation)
  (save-excursion
    (insert "{/*")
    (end-of-line)
    (insert "*/}")))

(defun ta-jsx-comment-or-uncomment-line ()
  "Comment element in `js-jsx-mode'."
  (interactive)
  (back-to-indentation)
  (if (looking-at "{/\\*")
      (ta-jsx-uncomment-line)
    (ta-jsx-comment-line)))

;;;; latex-mode

;;(require 'latex)
;;(require 'tex-site)

;;(add-to-list 'LaTeX-verbatim-environments "minted")
;;(add-to-list 'LaTeX-verbatim-macros-with-delims "mintinline{mysql}")

;;;; magit

(require 'magit)

(defun ta-magit-log-other-window ()
  "Show git logs in other windows."
  (interactive)
  (other-window 1)
  (call-interactively 'magit-log-current)
  (other-window 1))

(defun ta-magit-delete-other-windows (&rest r)
  "Intended to be advice of `magit-commit-create'."
  (delete-other-windows))

(advice-add 'magit-status :before 'ta-magit-delete-other-windows)
(advice-add 'magit-commit-create :before 'ta-magit-delete-other-windows)
(advice-add 'magit-commit-amend :before 'ta-magit-delete-other-windows)

(define-key magit-section-mode-map (kbd "C-i") 'magit-section-toggle)
(define-key magit-section-mode-map [C-tab]     'magit-section-cycle)
(define-key magit-section-mode-map (kbd "C-SPC") 'magit-section-cycle-global)
(define-key magit-section-mode-map (kbd   "p") 'magit-section-backward)
(define-key magit-section-mode-map (kbd   "n") 'magit-section-forward)
(define-key magit-section-mode-map (kbd "M-p") 'magit-section-backward-sibling)
(define-key magit-section-mode-map (kbd "M-n") 'magit-section-forward-sibling)

(define-key magit-mode-map (kbd "i") 'magit-section-up)

(define-key magit-status-mode-map (kbd "<prior>") 'insight-scroll-down-half-window)
(define-key magit-status-mode-map (kbd "<next>") 'insight-scroll-up-half-window)
(define-key magit-log-mode-map (kbd "<prior>") 'insight-scroll-down-half-window)
(define-key magit-log-mode-map (kbd "<next>") 'insight-scroll-up-half-window)

(global-set-key (kbd "C-x g") 'magit-status)

;;;; make-mode

(require 'make-mode)

(defun ta-newline-smart ()
  "Perform `newline-and-indent' or `newline' depending of the context."
  (interactive)
  (if (looking-back "\t")
      (progn
        (delete-char -1)
        (newline))
    (if (looking-back "\n\n")
        (newline)
      (newline)
      (indent-for-tab-command))))

(defun ta-makefile-tabs-settings ()
  "Turn on `indent-tabs-mode'.

Intended to be used in the hook `makefile-gmake-mode-hook'."
  (setq indent-tabs-mode t))

(add-hook 'makefile-gmake-mode-hook 'ta-makefile-tabs-settings)

(define-key makefile-gmake-mode-map (kbd "RET") 'ta-newline-smart)

;;;; markdown-mode

(require 'markdown-mode)
(require 'bicycle)
(require 'outline)
(require 'outline-spc)

(defun ta-markdown-tab ()
  "If on an outline header cycle visibility else insert \"\\t\".
We use `bicycle-cycle' to cycle the visibility.
See `outline-regexp' and `outline-level'."
  (interactive)
  (if (outline-on-heading-p)
      (call-interactively 'bicycle-cycle)
    (insert "\t")))

(defun ta-markdown-outline-level ()
  "Markdown mode `outline-level' function."
  (- (match-end 0) (match-beginning 0)))

(defun ta-markdown-mode-outline ()
  "Hook to turn on `outline-minor-mode'."
  (outline-minor-mode t)
  (outline-spc-mode t)
  (setq-local outline-regexp "##* \\|<details>")
  (setq-local outline-level #'ta-markdown-outline-level))

(add-hook 'markdown-mode-hook #'ta-markdown-mode-outline)

(define-key markdown-mode-map (kbd "C-M-i") nil)
(define-key markdown-mode-map (kbd "TAB") 'ta-markdown-tab)

;;;; mermaid-mode
(require 'mermaid-mode)

(add-to-list `auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

(defun mermaid-compile-on-save ()
  "Intended to be use as a hook of `after-save-hook'.

Live preview of the mermaid file being edited each time
you save the file."
  (when (eq major-mode 'mermaid-mode)
    (mermaid-compile)))

(add-hook 'after-save-hook 'mermaid-compile-on-save)

;;;; org-mode

;;;;; require

(require 'org)
(require 'plan)
(require 'smartparens)
(require 'company)

;;;;; Global

(setq org-tags-column -77)
(setq org-return-follows-link t)
(setq org-export-backends '(ascii beamer html icalendar latex md))
(setq org-hide-emphasis-markers t)
(set-default 'org-link-frame-setup '((file . find-file)))
(add-to-list 'org-file-apps '(directory . emacs))
(setq system-time-locale "C")
(setq org-log-done 'time)

;;;;; filtering

(defun ta-org-sparse-tree-beginning-of-line (&rest r)
  "Intended to be advice of `org-sparse-tree'."
  (beginning-of-line))

(advice-add 'org-sparse-tree :after 'ta-org-sparse-tree-beginning-of-line)

;;;;; src and babel

(require 'ob-js)
;; https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
(setq org-babel-js-function-wrapper
      "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")

(setq org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-languages '((js . t)
                             (shell . t)
                             (python . t)
                             (dot . t)
                             (mermaid . t)))

(defun ta-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "emacs-lisp"))
       (not (string= lang "dot"))
       (not (string= lang "mermaid"))
       (not (string= lang "shell"))))  ; don't ask for ditaa

(setq org-confirm-babel-evaluate 'ta-org-confirm-babel-evaluate)

;;;;; Tables

(setq org-table-tab-jumps-over-hlines t)

(defun ta-org-table-previous-row ()
  "Go to the previous row (same column) in the current table.
Before doing so, re-align the table if necessary."
  (interactive)
  (unless (org-at-table-hline-p)
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line))
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (when (and (org-at-table-p)
               (not (= (org-table-current-line) 1)))
      (previous-line)
      (unless (org-at-table-hline-p)
        (org-table-goto-column col)))))

(defun ta-org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item' or
`org-table-wrap-region', depending on context.

In table, `org-meta-return' calls `org-table-wrap-region' interactively
but it DOESN'T PASS the prefix arg.  So using `org-meta-return' in table
to run `org-table-wrap-region' does't work as expected.
Below you have a workaround to have full power of `org-table-wrap-region'
when calling `org-meta-return' in tables."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (if (org-at-table-p)
          (org-table-wrap-region arg)
        (call-interactively (cond (arg #'org-insert-heading)
                                  ((org-in-item-p) #'org-insert-item)
                                  (t #'org-insert-heading))))))

(defun ta-org-shiftmetadown (&optional _arg)
  "Drag the line at point down.
In a table, insert an empty row below the current line (this part
differs from the original `org-shiftmetadown' command).
On a clock timestamp, update the value of the timestamp like `S-<down>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point down."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetadown-hook))
   ((org-at-table-p) (org-table-insert-row 'below))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
                           (call-interactively 'org-timestamp-down)))
   (t (call-interactively 'org-drag-line-forward))))

(defun org-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment.

;; Tony Aldon (overwrite the original command `org-self-insert-command'.)
Add `ta-org-table-previous-row' to the commands after which we want
to blank table field if we start typing just after using it as `org-cycle',
`org-return', `org-shifttab', `org-ctrl-c-ctrl-c' commands."
  (interactive "p")
  (org-check-before-invisible-edit 'insert)
  (cond
   ((and org-use-speed-commands
         (let ((kv (this-command-keys-vector)))
           (setq org-speed-command
                 (run-hook-with-args-until-success
                  'org-speed-command-hook
                  (make-string 1 (aref kv (1- (length kv))))))))
    (cond
     ((commandp org-speed-command)
      (setq this-command org-speed-command)
      (call-interactively org-speed-command))
     ((functionp org-speed-command)
      (funcall org-speed-command))
     ((and org-speed-command (listp org-speed-command))
      (eval org-speed-command))
     (t (let (org-use-speed-commands)
          (call-interactively 'org-self-insert-command)))))
   ((and
     (= N 1)
     (not (org-region-active-p))
     (org-at-table-p)
     (progn
       ;; Check if we blank the field, and if that triggers align.
       (and (featurep 'org-table)
            org-table-auto-blank-field
            (memq last-command
                  '(ta-org-table-previous-row
                    org-cycle org-return org-shifttab org-ctrl-c-ctrl-c))
            (if (or (eq (char-after) ?\s) (looking-at "[^|\n]*  |"))
                ;; Got extra space, this field does not determine
                ;; column width.
                (let (org-table-may-need-update) (org-table-blank-field))
              ;; No extra space, this field may determine column
              ;; width.
              (org-table-blank-field)))
       t)
     (looking-at "[^|\n]*  |"))
    ;; There is room for insertion without re-aligning the table.
    (self-insert-command N)
    (org-table-with-shrunk-field
     (save-excursion
       (skip-chars-forward "^|")
       ;; Do not delete last space, which is
       ;; `org-table-separator-space', but the regular space before
       ;; it.
       (delete-region (- (point) 2) (1- (point))))))
   (t
    (setq org-table-may-need-update t)
    (self-insert-command N)
    (org-fix-tags-on-the-fly)
    (when org-self-insert-cluster-for-undo
      (if (not (eq last-command 'org-self-insert-command))
          (setq org-self-insert-command-undo-counter 1)
        (if (>= org-self-insert-command-undo-counter 20)
            (setq org-self-insert-command-undo-counter 1)
          (and (> org-self-insert-command-undo-counter 0)
               buffer-undo-list (listp buffer-undo-list)
               (not (cadr buffer-undo-list)) ; remove nil entry
               (setcdr buffer-undo-list (cddr buffer-undo-list)))
          (setq org-self-insert-command-undo-counter
                (1+ org-self-insert-command-undo-counter))))))))
;;;;; company, smartparens, org-indent-mode

(defun ta-org-smartparens ()
  "Intended to be used in the hook `org-mode-hook'."
  (sp-local-pair 'org-mode "*" "*" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "/" "/" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "_" "_" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "=" "=" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "~" "~" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "\"" "\"" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "'" "'" :actions '(wrap navigate))
  (show-smartparens-mode -1))

(defun ta-org-mode-company ()
  "Setup `company-mode' for `org-mode-hook'"
  (company-mode 1)
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  (setq company-minimum-prefix-length 4)
  (setq company-backends '(company-capf company-files company-dabbrev)))

(add-hook 'org-mode-hook #'ta-org-smartparens)
(add-hook 'org-mode-hook #'ta-org-mode-company)
(add-hook 'org-mode-hook #'org-indent-mode)

;;;;; keybindings

(define-key org-mode-map (kbd "C-e") nil)
(define-key org-mode-map (kbd "M-e") nil)
(define-key org-mode-map (kbd "C-a") nil)
(define-key org-mode-map (kbd "M-a") nil)

(define-key org-mode-map (kbd "M-m") 'ta-org-table-previous-row)
(define-key org-mode-map (kbd "C-<tab>") 'org-shifttab)
(define-key org-mode-map (kbd "<M-return>") 'ta-org-meta-return)
(define-key org-mode-map (kbd "M-S-<down>") 'ta-org-shiftmetadown)
(define-key org-mode-map (kbd "C-t") 'org-toggle-inline-images)

(setq org-use-speed-commands t)
(setq org-speed-commands-default nil)
(setq org-speed-commands-user
      '(("Outline Navigation")
        ("n" . (org-speed-move-safe 'org-next-visible-heading))
        ("p" . (org-speed-move-safe 'org-previous-visible-heading))
        ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
        ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
        ("i" . (org-speed-move-safe 'outline-up-heading))
        ("Sparse tree navigation")
        ("x" . previous-error)
        ("o" . next-error)
        ("Outline Structure Editing")
        ("." . org-toggle-narrow-to-subtree)
        ("@" . org-mark-subtree)
        ("`" . org-metaup)
        ("," . org-metadown)
        ("]" . org-shiftmetaright)
        ("[" . org-shiftmetaleft)
        (")" . org-metaright)
        ("(" . org-metaleft)
        ("+" . (progn (forward-char 1) (call-interactively
                                        'org-insert-heading-respect-content)))
        ("C" . org-copy-subtree)
        ("Meta Data Editing")
        ("t" . org-todo)
        (":" . org-set-tags-command)
        ;; ("c" . org-comment-dwim)
        ("Agenda Views etc")
        ("a" . org-agenda)
        ("/" . org-sparse-tree)
        ("%" . plan-sparse-tree-task-id)
        ("Clock commands")
        ("s" . org-clock-in)
        ("S" . org-clock-out)
        ;; ("C" . org-clock-cancel)
        ("v" . org-clock-goto)
        ("r" . org-clock-report)
        ("d" . org-clock-display)
        ("e" . org-set-effort)
        ("Columns")
        ("c" . org-columns)
        ("Misc")
        ("P" . org-set-property)
        ("?" . org-speed-command-help)))

;;;; outline-mode, outline-spc, bicycle

(require 'bicycle)
(require 'outline)
(require 'outline-spc)

(global-set-key (kbd "C-M-b") 'outline-previous-visible-heading)
(global-set-key (kbd "C-M-f") 'outline-next-visible-heading)
(global-set-key (kbd "C-SPC") 'ta-outline-toggle-global)

(setq outline-spc-default nil)
(setq outline-spc-user
      '(("Outline Navigation")
        ("n" . (outline-spc-move-safe 'outline-next-visible-heading))
        ("p" . (outline-spc-move-safe 'outline-previous-visible-heading))
        ("f" . (outline-spc-move-safe 'outline-forward-same-level))
        ("b" . (outline-spc-move-safe 'outline-backward-same-level))
        ("i" . (outline-spc-move-safe 'outline-up-heading))
        ("Outline Structure Editing")
        ("." . org-toggle-narrow-to-subtree)
        ("@" . outline-mark-subtree)
        ("`". outline-move-subtree-up)
        (",". outline-move-subtree-down)
        ("+". outline-insert-heading)))

;;;;; toggle global

(defun ta-outline-toggle-global ()
  "Toggle visibility of all outline (see `outline-mode') sections.

This command toggle between this following levels:
1. FOLDED:   Show only top level heading.
1. TREES:    Show all headings, treaing top-level code blocks
             as sections (i.e. their first line is treated as
             a heading).
2. ALL:      Show everything, except code blocks that have been
             collapsed individually (using a `hideshow' command
             or function).

This is a variant off (hack on) the `bicycle-cycle-global'."
  (interactive)
  (setq deactivate-mark t)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward outline-regexp nil t)
      (user-error "Found no heading"))
    (cond
     ((eq last-command 'outline-cycle-folded)
      (outline-hide-sublevels (bicycle--level))
      (outline-map-region
       (lambda ()
         (when (bicycle--top-level-p)
           (outline-show-branches)))
       (point-min)
       (point-max))
      (bicycle--message "TREES")
      (setq this-command 'outline-cycle-trees))
     ((eq last-command 'outline-cycle-trees)
      (outline-show-all)
      (bicycle--message "ALL"))
     (t
      (outline-map-region
       (lambda () (when (bicycle--top-level-p)
                    (outline-hide-subtree)))
       (point-min)
       (point-max))
      (bicycle--message "FOLDED")
      (setq this-command 'outline-cycle-folded)))))

;;;; Info-mode and help-mode

(require 'info)
(require 'help)
(require 'info-colors)

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(define-key Info-mode-map (kbd "d") 'insight-scroll-down-half-window)
(define-key Info-mode-map (kbd "s") 'insight-scroll-up-half-window)
(define-key help-mode-map (kbd "d") 'insight-scroll-down-half-window)
(define-key help-mode-map (kbd "s") 'insight-scroll-up-half-window)

;;;; isearch-mode and replace

(require 'isearch)

(setq search-whitespace-regexp ".*?")
(setq isearch-lazy-count t)
(setq isearch-repeat-on-direction-change t)
(setq search-invisible 'open)

(defadvice isearch-occur (before ta-occur-delete-other-windows activate)
  (delete-other-windows))

(defun ta-isearch-yank-sexp-at-point ()
  "Pull sexp at point into search string."
  (interactive)
  (isearch-yank-string (thing-at-point 'sexp)))

(defun ta-isearch-yank-word-at-point ()
  "Pull word at point into search string."
  (interactive)
  (isearch-yank-string (thing-at-point 'word)))

(defun ta-isearch-other-end ()
  "End current search in the opposite side of the match."
  (interactive)
  (isearch-done)
  (when isearch-other-end
    (goto-char isearch-other-end)))

;;;;; isearch outline headings

(require 'reveal)
(global-reveal-mode)

(defun ta-isearch-filter-outline (beg end)
  "Return non-nil if text between BEG and END is on an outline headline.
This function is intended to be used as `isearch-filter-predicate'.
See `outline-mode' and `outline-headings'"
  (save-match-data
    (save-excursion
      (goto-char beg)
      (outline-on-heading-p t))))

(defun ta-isearch-forward-outline ()
  "`isearch-forward' but matching only strings on outline headings.
See `outline-mode' and `outline-headings'."
  (interactive)
  (setq isearch-filter-predicate 'ta-isearch-filter-outline)
  (call-interactively 'isearch-forward))

(defun ta-isearch-backward-outline ()
  "`isearch-backward' but matching only strings on outline headings.
See `outline-mode' and `outline-headings'."
  (interactive)
  (setq isearch-filter-predicate 'ta-isearch-filter-outline)
  (call-interactively 'isearch-backward))

(defun ta-isearch-mode-end-outline ()
  "Set `isearch-filter-predicate' to its default value.
This function is intended to be used in the hook `isearch-mode-end-hook'."
  (setq isearch-filter-predicate 'isearch-filter-visible))

(add-hook 'isearch-mode-end-hook 'ta-isearch-mode-end-outline)

(global-set-key (kbd "M-a") 'ta-isearch-backward-outline)
(global-set-key (kbd "M-e") 'ta-isearch-forward-outline)

;;;;; replace

(require 'replace)

(define-key occur-mode-map (kbd "M-n") 'nil)

(global-set-key (kbd "M-}") 'query-replace)
(global-set-key (kbd "M-)") 'query-replace-regexp)

;;;;; mutiple buffers isearch

(require 'misearch)

(setq multi-isearch-pause nil)

(defun ta-misearch-same-mode ()
  "Isearch on live buffers with `major-mode' equal to current buffer."
  (interactive)
  (let* ((mode major-mode)
         (multi-isearch-next-buffer-function
          'multi-isearch-next-buffer-from-list)
         (buffers (--filter
                   (equal mode (with-current-buffer it major-mode))
                   (buffer-list))))
    (setq multi-isearch-buffer-list buffers)
    (isearch-forward nil t)
    (isearch-search-and-update)))

(global-set-key (kbd "C-M-n") 'ta-misearch-same-mode)

;;;;; isearch keybindings

(global-set-key (kbd "M-s") 'isearch-forward)
(global-set-key (kbd "M-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "M-s") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-r") 'isearch-repeat-backward)

(define-key isearch-mode-map (kbd "<prior>") 'isearch-beginning-of-buffer)
(define-key isearch-mode-map (kbd "<next>") 'isearch-end-of-buffer)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "M-.") 'ta-isearch-yank-sexp-at-point)
(define-key isearch-mode-map (kbd "M-,") 'ta-isearch-yank-word-at-point)
(define-key isearch-mode-map (kbd "M-t") 'ta-isearch-other-end)
(define-key isearch-mode-map (kbd "M-o") 'isearch-occur)
(define-key isearch-mode-map (kbd "M-c") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)
(define-key isearch-mode-map (kbd "M-e") 'isearch-toggle-regexp)
(define-key isearch-mode-map (kbd "M-}") 'isearch-query-replace)
(define-key isearch-mode-map (kbd "M-)") 'isearch-query-replace-regexp)

;;;; ivy and counsel

(require 'counsel)
(require 'ivy)

(counsel-mode 1)
(ivy-mode 1)

(setq ivy-height 8)
(setq ivy-wrap t)
(setq ivy-extra-directories '("./"))
(setq ivy-use-virtual-buffers nil)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-previous-history-element)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "<down>") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-e") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "M-p") 'ivy-reverse-i-search)

(define-key ivy-minibuffer-map (kbd "C-M-n") 'ta-counsel-rg-ivy-command)

;;;; php-mode

(eval-after-load 'php-mode '(require 'php-extras))

;; for php-extras Generate the hash table containing the PHP functions:
;; M-x load-library RET php-extras-gen-eldoc RET
;; M-x php-extras-generate-eldoc RET

(defun ta-php-mode-hook ()
  "Function to be used by `php-mode-hook'."
  (setq c-basic-offset 2))

(add-hook 'php-mode-hook 'ta-php-mode-hook)

;;;; region-active-spc-mode

(require 'region-active-spc)

(region-active-spc-mode t)

;;;; nxml-mode

(require 'nxml-mode)
(require 'outline)
(require 'outline-spc)

(defun ta-outline-nxml-mode-hook ()
  "Hook to turn on `outline-minor-mode'."
  (outline-minor-mode t)
  (outline-spc-mode t)
  (setq-local outline-regexp "<!--"))

(add-hook 'nxml-mode-hook 'ta-outline-nxml-mode-hook)

(define-key nxml-mode-map (kbd "TAB") 'bicycle-cycle)

;;;; sh-mode

(require 'sh-script)

(defun ta-sh-mode-company ()
  "Setup `company-mode' for `sh-mode-hook'"
  (company-mode 1)
  (setq company-backends '((company-capf company-files))))

(add-hook 'sh-mode-hook #'ta-sh-mode-company)

;;;; sql-mode

(require 'sql)
(require 'sql-indent)
(require 'sqlup-mode)

(add-hook 'sql-mode-hook 'sqlind-minor-mode)
(add-hook 'sql-mode-hook 'sqlup-mode)

;;;; smartparens and sexps

;;;;; smartparens

(require 'smartparens-config)

(smartparens-global-mode t)
(smartparens-global-strict-mode -1)
(show-smartparens-global-mode t)

(eval-after-load 'mhtml-mode '(require 'smartparens-html))
(eval-after-load 'LaTeX '(require 'smartparens-latex))
(add-to-list 'sp-navigate-consider-sgml-tags 'mhtml-mode)
(add-to-list 'sp-navigate-consider-sgml-tags 'js-mode)

(setq sp-navigate-interactive-always-progress-point nil)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
(setq sp-show-pair-from-inside nil)

;; When using smartparens in sgml-mode, there is a boring message that
;; appears when running sp-forward-sexp in an self-closing tag like
;; this <meta charset="utf-8/>".
(setq sp-message-width nil)

(defun indent-between-pair (&rest _ignored)
  "See: http://xenodium.com/emacs-smartparens-auto-indent/."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))

;;;;; Operate on sexps

(require 'handy)
(require 'hydra)
(require 'smartparens)
(require 'key-chord)

(defhydra handy-sexp
  (:pre (progn (if insight-mode (insight-mode -1))
               (set-cursor-color "#f92672"))
   :post (set-cursor-color "#26f9ad")
   :hint nil)
  ("t" handy-line/body :color blue)
  ;; miscellaneous
  ("~" set-mark-command)
  ("T" exchange-point-and-mark)
  ("r" join-line)
  ;; (";" sp-comment)
  ("_" handy-add-space :color blue)
  ;; kill
  ("M-d" sp-kill-sexp)
  ("DEL" sp-backward-kill-sexp)
  ("C" sp-copy-sexp)
  ("c" handy-avy-copy-past-sexp :color blue)
  ("C-y" sp-clone-sexp)
  ;; reshape
  ("," sp-change-enclosing :color blue)
  (";" sp-change-inner :color blue)
  (":" sp-split-sexp)
  ("M-:" sp-join-sexp)
  (">" sp-absorb-sexp)
  ("}" sp-emit-sexp)
  ("%" sp-convolute-sexp)
  ("M-f" sp-forward-slurp-sexp)
  ("M-b" sp-backward-slurp-sexp)
  ;; ("C-p" sp-add-to-previous-sexp)
  ;; ("C-n" sp-add-to-next-sexp)
  ;; ("M-f" sp-forward-barf-sexp)
  ;; ("M-b" sp-backward-barf-sexp)
  ("<left>" sp-splice-sexp-killing-backward)
  ("<right>" sp-splice-sexp-killing-forward)
  ("<up>" sp-raise-sexp)
  ("/" sp-splice-sexp)
  ;; motion
  ("M-p" sp-beginning-of-previous-sexp)
  ("M-n" sp-beginning-of-next-sexp)
  ("C-M-p" sp-end-of-previous-sexp)
  ("C-M-n" sp-end-of-next-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-next-sexp)
  ("p" sp-previous-sexp)
  ("u" sp-backward-up-sexp)
  ("i" sp-down-sexp)
  ("x" sp-up-sexp)
  ("y" sp-backward-down-sexp)
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("d" handy-sp-drag-backward)
  ("s" handy-sp-drag-forward)
  ;; parenthese type
  ("$" sp-show-enclosing-pair)
  ("{" sp-wrap-curly)
  ("(" sp-wrap-round)
  ("[" sp-wrap-square)
  ("M-r" sp-rewrap-sexp)
  ("]" sp-swap-enclosing-sexp)
  ;; ---
  ("M--" undo)
  ("q" nil))

(key-chord-define-global "-0" 'handy-sexp/body)
(key-chord-define-global "sr" 'handy-avy-copy-past-sexp)

;;;; term-mode

(require 'term)
(require 'eterm-256color)

(defun ta-ansi-term-bash ()
  (interactive)
  (let ((term-name
         (s-concat "term:.../" (f-filename default-directory) "/")))
    (ansi-term "/bin/bash" term-name)))

(defun ta-term-disable-hl-line-mode ()
  "Disable `hl-line-mode' in `term-mode'."
  (setq global-hl-line-mode nil))

(add-hook 'term-mode-hook #'eterm-256color-mode)
(add-hook 'term-mode-hook #'ta-term-disable-hl-line-mode)

(define-key term-raw-map (kbd "<left>") nil)
(define-key term-raw-map (kbd "<right>") nil)
(define-key term-raw-map (kbd "C-x") nil)
(define-key term-raw-map (kbd "M-x") nil)
(define-key term-raw-map (kbd "M-t") 'term-line-mode)
(define-key term-mode-map (kbd "M-t") 'term-char-mode)
(define-key term-raw-map (kbd "M-o") 'delete-window)
(define-key term-mode-map (kbd "M-o") 'delete-window)

(global-set-key (kbd "C-c t") 'ta-ansi-term-bash)

;;;; text-mode

(require 'text-mode)

(defun ta-text-mode-smartparens ()
  "Disable `show-smartparens-mode'.
Intented to be use in `text-mode-hook'"
  (show-smartparens-mode -1))

(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'ta-text-mode-smartparens)

(define-key text-mode-map (kbd "C-c C-l") 'ta-magit-log-other-window)

;;;; visual-line-mode

(require 'adaptive-wrap)

(defun ta-adaptative-wrap ()
  "respect indentation with visual-mode-line"
  (adaptive-wrap-prefix-mode))

(add-hook 'visual-line-mode-hook 'ta-adaptative-wrap)

;;;; wgrep-mode

(require 'wgrep)

(setq wgrep-auto-save-buffer t)

;;; Per package

;;;; avy

(require 'avy)

(setq avy-highlight-first t)
(setq avy-style 'at-full)
(setq avy-keys (listify-key-sequence "auieyxobnf()].,:;'csghrpqd"))

(defun ta-avy-goto-end-of-line ()
  "Call `avy-goto-char' with \"\n\" as argument."
  (interactive)
  (avy-goto-char ?\n))

(global-set-key (kbd "M-p") 'avy-goto-char)
(global-set-key (kbd "M-b") 'avy-goto-line)
(global-set-key (kbd "M-f") 'ta-avy-goto-end-of-line)

;;;; dump-jump
(require 'dumb-jump)

(setq dumb-jump-prefer-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;;; emacs lisp evaluation (pp, simple)

(require 'pp)
(require 'simple)

(defun ta-eval-expression (&optional arg)
  "Call `eval-expression'.
If called with universal argument, call `pp-eval-expression'."
  (interactive "P")
  (if arg
      (call-interactively 'pp-eval-expression)
    (call-interactively 'eval-expression)))

(global-set-key (kbd "<f1>") 'eval-defun)
(global-set-key (kbd "<f2>") 'eval-last-sexp)
(global-set-key (kbd "M-:") 'ta-eval-expression)

;;;; handy, drag-stuff (Operate on lines)

(require 'handy)
(require 'key-chord)
(require 'org)
(require 'drag-stuff)
(load "~/.emacs.d/.cask/28.0/elpa/drag-stuff-20161108.749/drag-stuff.el")

(defvar handy-line-active nil)

(defun handy-line-active ()
  "Toggle status of `handy-line-active'"
  (if handy-line-active
      (setq handy-line-active nil)
    (setq handy-line-active t)))

(defhydra handy-line
  (:pre (progn (if insight-mode (insight-mode -1))
               (set-cursor-color "#fa87ce"))
   :post (progn (set-cursor-color "#26f9ad")
                (handy-line-active))
   :hint nil)
  ("M-l" recenter-top-bottom)
  ("t" handy-sexp/body :color blue)
  (";" handy-line-comment)
  ("DEL" delete-backward-char)
  ("~" set-mark-command)
  ("m" exchange-point-and-mark)
  ;; action on line(s)
  ("!" flush-lines)
  ("?" keep-lines)
  ;; current line
  ("k" kill-line)
  ("l" (kill-line 0))
  ("x" handy-line-kill)
  ("y" handy-line-copy-paste-below)
  ("r" join-line)
  ("o" open-line)
  ("O" delete-blank-lines)
  ("," handy-cycle-spacing)
  ;; to insert text
  ("u" handy-line-add-above :color blue)
  ("]" handy-line-add-below :color blue)
  ("_" handy-add-space :color blue)
  ;; quick motions
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("i" back-to-indentation)
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("M-f" forward-word)
  ("M-b" backward-word)
  ("M-e" org-forward-sentence)
  ("M-a" org-backward-sentence)
  ;; drag stuff
  ("d" drag-stuff-up)
  ("s" drag-stuff-down)
  ("<left>" drag-stuff-left)
  ("<right>" drag-stuff-right)
  ;; clean/undo/nil
  ("M--" undo)
  ("q" nil))

(defadvice move-beginning-of-line (before move-beginning-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice move-end-of-line (before move-end-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice handy-line/body (before handy-line-advice activate)
  (handy-line-active))

(key-chord-define-global "ld" 'handy-line/body)

;;;; help

(require 'help-fns)
(require 'mouse)

(defun ta-describe-thing-at-point ()
  "Display the full documentation of the `thing-at-point'

that is either a function or a variable.
Return nil if the symbol of the `thing-at-point' is neither a function
nor a variable."
  (interactive)
  (when-let* ((symbol (symbol-at-point))
              (symbol-n (symbol-name symbol)))
    (when (and (eq major-mode 'org-mode)
               (s-starts-with-p "~" symbol-n)
               (s-ends-with-p "~" symbol-n))
      (setq symbol (->> symbol-n
                        (s-chop-prefix "~")
                        (s-chop-suffix "~")
                        (intern))))
    (describe-symbol symbol)))

(defun ta-mouse-describe-thing-at-point ()
  "Call `ta-describe-thing-at-point' at cursor position."
  (interactive)
  (call-interactively 'mouse-set-point)
  (call-interactively 'ta-describe-thing-at-point))

(global-set-key (kbd "C-d") 'ta-describe-thing-at-point)
(global-set-key (kbd "<C-down-mouse-3>") 'ta-mouse-describe-thing-at-point)

;;;; quick-access

(require 'quick-access)

(setq quick-access-alist
      '(("ie users" . "~/work/inside-emacs/social-medias/users.org")
        ("ie social-medias" . "~/work/inside-emacs/social-medias/social-medias.org")
        ("ie top-up/README" . "~/work/inside-emacs/top-up/README.md")
        ("ie videos/README" . "~/work/inside-emacs/videos/README.org")
        ("ie social-medias/" . "~/work/inside-emacs/social-medias/")
        ("ie top-up/" . "~/work/inside-emacs/top-up/")
        ("ie videos/" . "~/work/inside-emacs/videos/")
        ("rp weekly-reports" . "~/work/rich-project/reports/weekly-reports-2021.org")
        ("rp diary" . "~/work/rich-project/diary/diary.org")
        ("rp life-map" . "~/work/rich-project/targets-2021-06-07--2021-11-21.png")
        ("rp targets" . "~/work/rich-project/life-map.png")
        ("notes" . "~/work/notes.org")
        ("contact" . "~/work/contact/contact.org")
        ("tmp" . "~/work/tmp/")
        ("tonyaldon.com" . "~/work/apps/tonyaldon.com/")
        ("settings emacs" . "~/work/settings/emacs.d/.emacs.d/settings/")
        ("settings linux" . "~/work/settings/")
        ("tricks/directory" . "~/work/learning/tricks/")
        ("tricks/ideas" . "~/work/learning/tricks/ideas.org")
        ("expenses" . "~/life/home/expenses/expenses.csv")
        ("apps/emacs" . "~/work/learning/apps/emacs/")))

;;;; dired, wdired and bfs

;;;;; dired and wdired

(require 'dired)
(require 'dired-toggle-sudo)

(setq dired-keep-marker-rename t)
(setq dired-clean-up-buffers-too t)
(setq dired-clean-confirm-killing-deleted-buffers t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(setq-default dired-listing-switches "-al --group-directories-first")

(defun ta-dired-auto-revert ()
  "Set `auto-revert-mode' in `dired-mode' buffers."
  (auto-revert-mode 1)
  (set (make-local-variable 'auto-revert-verbose) nil))

(add-hook 'dired-mode-hook 'ta-dired-auto-revert)

(require 'wdired)

(setq wdired-allow-to-change-permissions nil)
(setq wdired-create-parent-directories t)

(require 'dired-open)

(setq dired-open-extensions
      '(("pdf" . "evince")
        ("jpg" . "eog")
        ("jpeg" . "eog")
        ("png" . "eog")))

;;;;; dired-mode-map

(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "C-M-p") nil)
(define-key dired-mode-map (kbd "C-M-n") nil)
(define-key dired-mode-map (kbd "M-s") 'isearch-forward)
(define-key dired-mode-map (kbd "M-r") 'isearch-backward)

(define-key dired-mode-map (kbd "b") 'dired-up-directory)
(define-key dired-mode-map (kbd "f") 'dired-open-file)
(define-key dired-mode-map (kbd "p") 'dired-previous-line)
(define-key dired-mode-map (kbd "n") 'dired-next-line)

(define-key dired-mode-map (kbd "+") 'dired-create-directory)

(define-key dired-mode-map (kbd ".") 'dired-mark)
(define-key dired-mode-map (kbd "t") 'dired-toggle-marks)
(define-key dired-mode-map (kbd "u") 'dired-unmark)
(define-key dired-mode-map (kbd "U") 'dired-unmark-all-marks)

(define-key dired-mode-map (kbd "F") 'dired-do-find-marked-files)

(define-key dired-mode-map (kbd "r") 'dired-do-rename)
(define-key dired-mode-map (kbd "c") 'dired-do-copy)
(define-key dired-mode-map (kbd "C") 'dired-copy-filename-as-kill)

(define-key dired-mode-map (kbd "d") 'dired-flag-file-deletion)
(define-key dired-mode-map (kbd "x") 'dired-do-flagged-delete)

(define-key dired-mode-map (kbd "i") 'dired-show-file-type)
(define-key dired-mode-map (kbd "!") 'dired-do-shell-command)
(define-key dired-mode-map (kbd "g") 'revert-buffer)

(define-key dired-mode-map (kbd "S") 'dired-toggle-sudo)
(define-key dired-mode-map (kbd "TAB") 'dired-hide-details-mode)

;;;; refactor

(require 'refactor)
(require 'hydra)

(defhydra hydra-refactor
  (:hint nil)
  ("M-t" refactor-write-mode :color blue)
  ("a" refactor-abort-changes :color blue)
  ("e" refactor-exit :color blue)
  ("f" refactor-finish-edit :color blue)
  ("M--" undo)
  ("q" nil))

(global-set-key (kbd "M-t") 'hydra-refactor/body)

;;;; rg

(require 'rg)

(setq rg-ignore-case 'smart)

(rg-define-search ta-rg-ask-project-dir
  "Search for a string (given by the user at the prompt) in files matching
the current file under the project root directory."
  :query ask
  :format regexp
  :files current
  :dir project)

(rg-define-search ta-rg-ask-current-dir
  "Search for a string (given by the user at the prompt) in files matching
the current file under the current directory."
  :query ask
  :format regexp
  :files current
  :dir current)

(defun ta-rg-ask (&optional arg)
  "Run ripgrep only asking for the regex to be matched.
The default searches for regex in files matching current file
under project root directory.

With \\[universal-argument] prefix, search is done in
current dir instead of project root."
  (interactive "p")
  (if (equal arg 4)
      (call-interactively 'ta-rg-ask-current-dir)
    (call-interactively 'ta-rg-ask-project-dir)))

(defadvice rg-run (before ta-rg-delete-other-windows activate)
  (delete-other-windows))

(defun ta-rg-rerun-toggle-hidden ()
  "Rerun last search with toggled '--hidden' flag."
  (interactive)
  (rg-rerun-toggle-flag "--hidden"))

;;;;; override rg

;; override the original header line function
;; mostly to include information about matching in hidden files
(defun rg-header-render-toggle (ignored)
  "Return a fontified toggle symbol.
If IGNORED is non nil, render \"y\" string, otherwise render \"no\"
string.

Tony Aldon: I override this function because of the semantic of the
words.  The default words \"on\" and \"off\" used in the header line
are not clear for me."
  `(:eval (let* ((ignored ,ignored)
                 (value (if ignored "y" "no"))
                 (face (if ignored 'rg-toggle-on-face 'rg-toggle-off-face)))
            (propertize value 'font-lock-face `(bold ,face)))))

(defun rg-create-header-line (search full-command)
  "Create the header line for SEARCH.
If FULL-COMMAND specifies if the full command line search was done.

Tony Aldon: I override this function because I want to rename the
labels and add a new label for the option \"--hidden\" of \"ripgrep\"
to show me if the matches include those in the hidden files.  Note
that by default, `rg' skips hidden files and directory."
  (let ((itemspace " "))
    (setq header-line-format
          (if full-command
              (list (rg-header-render-label "command line") "no refinement")
            (list
             (rg-header-mouse-action 'rg-rerun-toggle-rexexp-literal "Toggle literal/regexp"
                                     (rg-header-render-label `((rg-search-literal ,search)
                                                               ("literal" rg-literal-face)
                                                               ("regexp" rg-regexp-face))))
             (rg-header-mouse-action 'rg-rerun-change-literal "Change search string"
                                     `(:eval (rg-search-pattern ,search))) itemspace
             (rg-header-render-label "files")
             (rg-header-mouse-action 'rg-rerun-change-files "Change file types"
                                     `(:eval (rg-search-files ,search))) itemspace
             ;; label semantic: r-case   -> respect case
             ;;                 r-git    -> respect .gitignore
             ;;                 s-hidden -> search matches in hidden files
             (rg-header-render-label "r-case|r-git|s-hidden")
             (rg-header-mouse-action 'rg-rerun-toggle-case "Toggle case"
                                     (rg-header-render-toggle
                                      `(not (member "-i" (rg-search-flags ,search))))) itemspace
             (rg-header-mouse-action 'rg-rerun-toggle-ignore "Toggle ignore"
                                     (rg-header-render-toggle
                                      `(not (member "--no-ignore" (rg-search-flags ,search))))) itemspace
             (rg-header-mouse-action 'ta-rg-rerun-toggle-hidden "Toggle hidden"
                                     (rg-header-render-toggle
                                      `(member "--hidden" (rg-search-flags ,search)))) itemspace
             (rg-header-render-label "hits")
             '(:eval (format "%d" rg-hit-count)))))))


;; override file path rendering and match
;; mostly to add the filename without its directory
;; (default) File: path-to-file
;; (new)     File: [filename] path-to-file
(defun rg-filter ()
  "Handle match highlighting escape sequences inserted by the rg process.
This function is called from `compilation-filter-hook'.

Tony Aldon: I override this function because I want to
modify the rendering of the lines containing the path
of the file where matches are found.  Precisely, I want
to add the filename without its directory:
(default) File: path-to-file
(new)     File: [filename] path-to-file."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      (when (zerop rg-hit-count)
        (newline))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Add File: in front of filename
        (when rg-group-result
          (while (re-search-forward "^\033\\[[0]*m\033\\[35m\\(.*?\\)\033\\[[0]*m$" end 1)
            (replace-match (concat (propertize "File:"
                                               'rg-file-message t
                                               'face nil
                                               'font-lock-face 'rg-file-tag-face)
                                   " "
                                   (propertize (concat "["
                                                       (file-name-nondirectory (match-string 1))
                                                       "]")
                                               'font-lock-face 'rg-filename-face)
                                   " "
                                   (propertize (match-string 1)
                                               'face nil
                                               'font-lock-face 'rg-filename-face))
                           t t))
          (goto-char beg))

        ;; Highlight rg matches and delete marking sequences.
        (while (re-search-forward "\033\\[[0]*m\033\\[[3]*1m\033\\[[3]*1m\\(.*?\\)\033\\[[0]*m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face 'rg-match-face)
                         t t)
          (cl-incf rg-hit-count))

        (rg-format-line-and-column-numbers beg end)

        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[0mK]" end 1)
          (replace-match "" t t))

        (goto-char beg)
        (run-hooks 'rg-filter-hook)))))

(setq wgrep-rg-grouped-result-file-regexp "^File: \\[[^]]+] \\(.*\\)$")

(defun rg-match-grouped-filename ()
  "Match filename backwards when a line/column match is found in grouped output mode.

Tony Aldon: I override this function to allow (rg) to
follow working well with the changes I made in `rg-filter'."
  (save-match-data
    (save-excursion
      (when (re-search-backward "^File: \\[[^]]+] \\(.*\\)$" (point-min) t)
        (list (match-string 1))))))

;;;;; rg keybindings

(define-key rg-mode-map (kbd "C-o") nil)
(define-key rg-mode-map (kbd "TAB") 'compilation-display-error)

(define-key rg-mode-map (kbd "c") 'rg-rerun-toggle-case)
(define-key rg-mode-map (kbd "i") 'rg-rerun-toggle-ignore)
(define-key rg-mode-map (kbd "h") 'ta-rg-rerun-toggle-hidden)
(define-key rg-mode-map (kbd "d") 'rg-rerun-change-dir)
(define-key rg-mode-map (kbd "f") 'rg-rerun-change-files)
(define-key rg-mode-map (kbd "r") 'rg-rerun-change-regexp)
(define-key rg-mode-map (kbd "t") 'rg-rerun-change-literal)
(define-key rg-mode-map (kbd "g") 'rg-recompile)

(define-key rg-mode-map (kbd "-") 'rg-back-history)
(define-key rg-mode-map (kbd "+") 'rg-forward-history)
(define-key rg-mode-map (kbd "(") 'rg-prev-file)
(define-key rg-mode-map (kbd ")") 'rg-next-file)
(define-key rg-mode-map (kbd "p") 'previous-error-no-select)
(define-key rg-mode-map (kbd "n") 'next-error-no-select)

(global-set-key (kbd "M-n") 'ta-rg-ask)

;;;; yasnippet

(require 'yasnippet)

(yas-global-mode 1)
(define-key yas-keymap (kbd "M-d")
  (yas-filtered-definition yas-maybe-skip-and-clear-field))

;;; Inside Emacs Setup

(require 'screencast)
(require 'ie-story-generate)
(require 'ie-last-video)

(setq ie-last-video-main-dir "~/work/inside-emacs/videos/")
(setq screencast-hook-to-remove-alist nil)
(setq screencast-display-buffer-alist display-buffer-alist)

(global-set-key (kbd "C-c i") 'ie-last-video-find-readme)

;;; TODO: to dispatch in appropriate section

(defun ta-copy-buffer-file-name ()
  "Push current `buffer-file-name' to the `kill-ring'."
  (interactive)
  (kill-new (buffer-file-name)))

(defun ta-find-file-notes ()
  (interactive)
  (find-file "~/work/notes.org"))

;;; Footer

(provide 'setup-init)
