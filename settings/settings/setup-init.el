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

;;;; Buffer editing

(set-language-environment "UTF-8")
(setq save-interprogram-paste-before-kill t)

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

;;;; Window layout
;;;;; display-buffer-alist

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

;;;; TODO: abbrev-mode
;; - emacs abbrevation: in text mode,
;;   - Diary: fill improvement and learning (Tony Aldon)
;;   - Diary: set focus and tasks (Tony Aldon)
;;   - Fill improvement and learning (Tony Aldon)
;;   - Set focus and tasks (Tony Aldon)

;; abbrev-file-name
;; write-abbrev-file
;; edit-abbrevs
;; /home/tony/work/learning/apps/emacs/lisp/abbrev.el
;; https://protesilaos.com/dotemacs/#h:57dcf193-0c4e-4ee6-9b2d-6892558b0a84

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

;;;; insight-mode

(require 'insight)
(setq insight-cursor-color "#fd971f")
(insight-use-cursor-color)
(insight-set-window-advices)
(define-key insight-mode-map (kbd "t") 'handy-line/body)

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
(define-key makefile-gmake-mode-map (kbd "M-n") 'windmove-down)
(define-key makefile-gmake-mode-map (kbd "M-p") 'windmove-up)

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

;;;; Info-mode

(require 'info-colors)

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;;;; isearch-mode

(require 'isearch)

(defadvice isearch-occur (before ta-occur-delete-other-windows activate)
  (delete-other-windows))

;;;;; TODO
(setq search-whitespace-regexp ".*?")
(setq isearch-lazy-count t)



(setq search-invisible 'open) ; default
;; (setq search-invisible 'nil) could be interesting to make a search only on the outline headings

;; (setq isearch-other-end nil)

;; isearch-mode-hook
;; isearch-update-post-hook
;; isearch-mode-end-hook
;; isearch-mode-end-hook-quit

;; isearch-mode-map

;; (setq search-highlight t)
;; (setq search-whitespace-regexp ".*?")
;; (setq isearch-lax-whitespace t)
;; (setq isearch-regexp-lax-whitespace nil)
;; (setq isearch-lazy-highlight t)
;; ;; All of the following variables were introduced in Emacs 27.1.
;; (setq lazy-count-prefix-format nil)
;; (setq lazy-count-suffix-format " (%s/%s)")
;; (setq isearch-yank-on-move 'shift)
;; (setq isearch-allow-scroll 'unlimited)

;; (defun prot/isearch-other-end ()
;;     "End current search in the opposite side of the match.
;; Particularly useful when the match does not fall within the
;; confines of word boundaries (e.g. multiple words)."
;;     (interactive)
;;     (isearch-done)
;;     (when isearch-other-end
;;       (goto-char isearch-other-end)))



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

;;;; smartparens

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

;;;; term-mode

(require 'term)
(require 'eterm-256color)

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

;;;; org-mode

(defun ta-org-smartparens ()
  "Intended to be used in the hook `org-mode-hook'."
  (sp-local-pair 'org-mode "*" "*" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "/" "/" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "_" "_" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "=" "=" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "~" "~" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "\"" "\"" :actions '(wrap navigate))
  (sp-local-pair 'org-mode "'" "'" :actions '(wrap navigate)))

(add-hook 'org-mode-hook 'ta-org-smartparens)


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
;;;; ace-hacks
(require 'ace-hacks)

(declare-function counsel-quick-access "ext:quick-access")
(declare-function quick-access-get-filename "ext:quick-access")
(declare-function ivy--directory "ext:ivy")
(declare-function  ivy--switch-buffer-action "ext:ivy")

(setq ace-hacks-ivy-callers-alist
      '((ivy-switch-buffer . ivy--switch-buffer-action)
        (ivy-switch-buffer-other-window . ivy--switch-buffer-action)
        (counsel-find-file . (lambda (file)
                               (find-file (expand-file-name file ivy--directory))))
        (counsel-quick-access . (lambda (selection)
                                  (find-file (quick-access-get-filename selection))))
        (counsel-rg . counsel-git-grep-action)))


;;;; dump-jump
(require 'dumb-jump)

(setq dumb-jump-prefer-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

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

;;;; rg

(require 'rg)

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
  (interactive "P")
  (cond
   ((equal arg 4) (call-interactively 'ta-rg-ask-current-dir))
   (t (call-interactively 'ta-rg-ask-project-dir))))

(defadvice rg-run (before ta-rg-delete-other-windows activate)
  (delete-other-windows))

;;;;; outline

(defun ta-outline-rg-mode ()
  "Set up `outline-mode' for `rg-mode'. See `outline-regexp'."
  (outline-minor-mode t)
  (setq-local outline-regexp "File:"))

(add-hook 'rg-mode-hook 'ta-outline-rg-mode)

(define-key rg-mode-map (kbd "TAB") 'bicycle-cycle)

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

;;; TODO: to dispatch in appropriate setup files
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

(defun ta-copy-buffer-file-name ()
  "Push current `buffer-file-name' to the `kill-ring'."
  (interactive)
  (kill-new (buffer-file-name)))

(defun ta-toggle-create-lockfiles ()
  "Toggle the value of `create-lockfiles' interactively."
  (interactive)
  (setq create-lockfiles (not create-lockfiles))
  (message "create-lockfiles set to: %s" create-lockfiles))

(defun ta-find-file-notes ()
  (interactive)
  (find-file "~/work/notes.org"))

;;; Footer

(provide 'setup-init)
