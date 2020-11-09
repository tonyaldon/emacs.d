;;; Global

;;;; Appearance

;;;;; Cursor

(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 2))


;;;;; Font

(set-face-attribute 'default nil :family "DejaVu Sans Mono")
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;;;;; Frame

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
(setq frame-resize-pixelwise t)

;;;;; Lines

(global-linum-mode -1)
(setq linum-format " %7i ")

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
(setq keycast-window-predicate  'moody-window-active-p)
(setq keycast-remove-tail-elements nil)

(setq keycast-substitute-alist
      '((self-insert-command "." "self-insert-command")
        (org-self-insert-command "." "org-self-insert-command")
        (outline-self-insert-command "." "outline-self-insert-command")))

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
(pending-delete-mode t) ;; FIXME: no longer work with my configuration
(setq save-interprogram-paste-before-kill t)
(setq-default tab-width 2)
(setq fill-column 80)

;;;; Window layout
;;;;; display-buffer-alist
(setq window-sides-vertical nil)

(setq ta-display-buffer-alist-monitor
      '(;; side windows
        ("\\*Help.*"
         (display-buffer-in-side-window)
         (window-height . 0.5)
         (side . top)
         (slot . -1))
        ("\\*scratch.*"
         (display-buffer-in-side-window)
         (window-height . 0.5)
         (side . top)
         (slot . 1))
        ("\\*Messages.*\\|\\*Warnings.*\\|\\*Backtrace.*"
         (display-buffer-in-side-window)
         (window-width . 0.36)
         (side . left)
         (slot . 1))
        ("\\*YASnippet Tables by NAMEHASH\\*\\|\\*YASnippet Tables\\*"
         (display-buffer-in-side-window)
         (window-width . 0.36)
         (side . left)
         (slot . 2))
        (".*occur.*\\|\\*grep.*\\|\\*rg.*\\|*Occur.*"
         (display-buffer-in-side-window)
         (window-width . 0.5)
         (side . left)
         (slot . 2))
        ;; Normal windows
        ("\\*info.*\\|\\*Apropos.*"
         (display-buffer-same-window))
        ("magit:.*\\|magit-diff:.*"
         (display-buffer-in-direction)
         (direction . left))
        ("magit-log:.*"
         (display-buffer-below-selected))
        ("\\*terminal.*"
         (display-buffer-below-selected))))

(setq ta-display-buffer-alist-half-monitor
      '(;; side windows
        ("\\*Help.*"
         (display-buffer-in-side-window)
         (window-height . 0.5)
         (side . top)
         (slot . -1))
        ("\\*scratch.*"
         (display-buffer-in-side-window)
         (window-height . 0.5)
         (side . top)
         (slot . 1))
        ("\\*Messages.*\\|\\*Warnings.*\\|\\*Backtrace.*"
         (display-buffer-in-side-window)
         (window-height . 0.5)
         (side . bottom)
         (slot . -1))
        ("\\*YASnippet Tables by NAMEHASH\\*\\|\\*YASnippet Tables\\*"
         (display-buffer-in-side-window)
         (window-height . 0.5)
         (side . bottom)
         (slot . 1))
        (".*occur.*\\|\\*grep.*\\|\\*rg.*\\|*Occur.*"
         (display-buffer-in-side-window)
         (window-height . 0.5)
         (side . bottom)
         (slot . 1))
        ;; Normal windows
        ("\\*info.*\\|\\*Apropos.*"
         (display-buffer-same-window))
        ("magit:.*\\|magit-diff:.*"
         (display-buffer-below-selected))
        ("magit-log:.*"
         (display-buffer-below-selected))
        ("\\*terminal.*"
         (display-buffer-below-selected))))

(setq ta-frame-width-monitor 1916)

(defun ta-frame-set-display-alist ()
  "Set the `display-buffer-alist' regarding the window configuration.

This function should called whenever the window configuration changes
(`window-configuration-change-hook')."
  (if (eq (frame-native-width) ta-frame-width-monitor)
      (setq display-buffer-alist ta-display-buffer-alist-monitor)
    (setq display-buffer-alist ta-display-buffer-alist-half-monitor)))

(add-hook 'window-configuration-change-hook 'ta-frame-set-display-alist)
;;;; PATH

(setenv "PATH" (concat "~/.local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.local/bin")

;;;; Other

(setq create-lockfiles nil)

(setq browse-url-browser-function 'browse-url-chromium)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(add-to-list `auto-mode-alist '("\\.svg\\'" . fundamental-mode))
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'recentf)

(save-place-mode t)
(global-auto-revert-mode 1)
(recentf-mode 1)

(setq save-place-file "~/.emacs.d/places")
(setq recentf-save-file "~/.emacs.d/recentf")
(setq recentf-max-saved-items 25)

(setq scroll-preserve-screen-position 1)
(setq scroll-conservatively 100)
(setq recenter-positions '(middle top bottom))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Per Mode

;;;; css-mode

(setq css-indent-offset 2)

(require 'emmet-mode)
;; bind the command: emmet-expand-line

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

;; todo: add company completion

;;;; i3-mode

(require 'i3)

;;;; insight-mode

(require 'insight)
(setq insight-cursor-color "#fd971f")
(insight-use-cursor-color)
(insight-set-window-advices)
(define-key insight-mode-map (kbd "t") 'hydra-lines/body)

;;;; js-mode
;;;;; Packages

(require 'outline)
(require 'js)

;;;;; Global

(setq js-indent-level 2)

;;;;; tide-mode

(defun ta-tide-mode-hook ()
  "Setup function for tide."
  (interactive)
  (tide-setup))

;;;;; Outline

(defun ta-outline-js-mode-hook ()
  "Set up `outline-mode' and `bicycle'.  Specifically,

the variable `outline-regexp'."
  (outline-minor-mode t)
  (setq outline-regexp (concat
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

;;;;; Hooks

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

(defun ta-magit-delete-other-windows-2 (&rest r)
  "Delete other windows when `current-buffer' is not a magit buffer."
  (unless (s-contains-p "magit" (buffer-name)) (delete-other-windows)))

(defun ta-magit-delete-other-windows-1 (&rest r)
  "Inted to be advice of `magit-commit-create'."
  (delete-other-windows))

(advice-add 'magit-status :before 'ta-magit-delete-other-windows-2)
(advice-add 'magit-commit-create :before 'ta-magit-delete-other-windows-1)

;; (advice-remove 'magit-status 'ta-delete-other-windows-when-non-magit-buffer)

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

(defun ta-makefile-gmake-mode-hook ()
  "Hook for `makefile-gmake-mode'."
  (setq tab-width 8))

(add-hook 'makefile-gmake-mode-hook 'ta-makefile-gmake-mode-hook)

(define-key makefile-gmake-mode-map (kbd "RET") 'ta-newline-smart)
(define-key makefile-gmake-mode-map (kbd "M-n") 'windmove-down)
(define-key makefile-gmake-mode-map (kbd "M-p") 'windmove-up)

;;;; mini-frame
(require 'mini-frame)

(setq mini-frame-ignore-commands '(swiper
                                   swiper-isearch-toggle
                                   swiper-all
																	 swiper-thing-at-point
                                   revert-buffer
                                   ta-zap-back-to-char
                                   counsel-outline
                                   counsel-rg
                                   isearch-edit-string
                                   counsel-fzf
                                   counsel-yank-pop))
(setq mini-frame-show-parameters
      '((left . 0.5)
        (top . 0.08)
        (width . 0.6)
        (height . 1)
        (background-color . "#151515")))
(setq mini-frame-internal-border-color "#01676b")
(setq mini-frame-resize t)

(defun mini-frame--make-frame (parameters)
  "Make frame with common parameters and PARAMETERS.

Note: Modify the internal-border-width of the frame."
  (let ((frame (make-frame (append parameters
                                   '((visibility . nil)
                                     (user-position . t)
                                     (user-size . t)
                                     (keep-ratio . t)
                                     (undecorated . t)
                                     (desktop-dont-save . t)
                                     (internal-border-width . 2) ; default value was 3
                                     (drag-internal-border . t))))))
    (set-face-background 'fringe nil frame)
    (when mini-frame-internal-border-color
      (set-face-background 'internal-border mini-frame-internal-border-color frame))
    frame))


;; 1. must be turn on after setting the preceding variables.
;; 2. warning, if you turn on the mode in your init file directly,
;; when you start the emacs daemon (without inside a frame) you get the error:
;; Warning (initialization): An error occurred while loading ‘/home/tony/.emacs.d/init.el’:
;; error: Unknown terminal type
;; Workaround: turn it on in the after-make-frames-functions list.
(add-to-list 'after-make-frame-functions 'mini-frame-mode t)

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


;;;; screencast-mode
(require 'screencast)

(setq screencast-hook-to-remove-alist
      '((window-configuration-change-hook . ta-frame-set-display-alist)))

(setq screencast-display-buffer-alist '())

;;;; sql-mode

(require 'sql)
(require 'sql-indent)
(require 'sqlup-mode)

(add-hook 'sql-mode-hook 'sqlind-minor-mode)
(add-hook 'sql-mode-hook 'sqlup-mode)

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
                                  (find-file (quick-access-get-filename selection))))))


;;;; dump-jump
(require 'dumb-jump)

(setq dumb-jump-prefer-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;;; quick-access

(require 'quick-access)

(setq quick-access-alist
      '(("notes" . "~/work/notes.org")
        ("extra" . "~/work/extra.org")
        ("tmp" . "~/work/tmp/")
        ("Jack Inside - private" . "~/work/jackinside/private/")
        ("i3 config" . "~/work/settings/i3/.config/i3/config")
        ("settings emacs" . "~/work/settings/emacs.d/.emacs.d/settings/")
        ("settings linux" . "~/work/settings/")
        ("learning" . "~/work/learning/")
        ("tricks emacs" . "~/work/learning/tricks/org/emacs.org")
        ("tricks clojure" . "~/work/learning/tricks/org/clojure.org")
        ("tricks linux" . "~/work/learning/tricks/org/linux.org")
        ("tricks git" . "~/work/learning/tricks/org/git.org")
        ("tricks miscellaneous" . "~/work/learning/tricks/org/miscellaneous.org")
        ("tricks media" . "~/work/learning/tricks/org/media.org")
        ("tricks frontend" . "~/work/learning/tricks/org/frontend.org")
        ("csv - my videos" . "~/work/learning/videos/videos.csv")
        ("csv - my expenses" . "~/life/home/expenses/expenses.csv")
        ("videos - programming-sessions" . "~/work/videos/programming-sessions/")
        ("videos - youtube/README" . "~/work/videos/youtube/README.org")
        ("inside-emacs/README" . "~/work/videos/inside-emacs/README.org")
        ("inside-emacs/directory" . "~/work/videos/inside-emacs/")
        ("inside-emacs/repository/README" . "~/work/learning/inside-emacs/README.md")
        ("apps/emacs" . "~/work/learning/apps/emacs/")
        ("Jack Inside" . "~/work/jackinside/")
        ("Jack Inside (handbook)" . "~/work/jackinside/handbook/")
        ("Jack Inside (diary)" . "~/work/jackinside/handbook/diary.org")
				("foreign languages" . "~/work/learning/foreign-languages/")
				("foreign languages - Anglais Modern" . "~/work/learning/foreign-languages/Anglais-Moderne.csv")))

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
  (setq outline-regexp "File:"))

(add-hook 'rg-mode-hook 'ta-outline-rg-mode)

(define-key rg-mode-map (kbd "TAB") 'bicycle-cycle)

;;;; yasnippet

(require 'yasnippet)

(yas-global-mode 1)
(define-key yas-keymap (kbd "M-d")
  (yas-filtered-definition yas-maybe-skip-and-clear-field))

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

(defun ta-switch-keyboard-layout ()
  "Switch keyboard layout variant between\"takbl\" and \"takbl fr\"."
  (interactive)
  (let ((variant (shell-command-to-string "setxkbmap -query | grep variant | awk -F' '  '{ print $2 }'")))
    (if (string= variant "fr\n")
        (progn
          (shell-command-to-string "setxkbmap -layout takbl")
          (message "takbl"))
      (shell-command-to-string "setxkbmap -layout takbl -variant fr")
      (message "takbl - fr"))))

(defun ta-toggle-create-lockfiles ()
  "Toggle the value of `create-lockfiles' interactively."
  (interactive)
  (setq create-lockfiles (not create-lockfiles))
  (message "create-lockfiles set to: %s" create-lockfiles))

(defun ta-find-file-notes ()
  (interactive)
  (find-file "~/work/notes.org"))

;;;; Git (related to)
(defun ta-pre-format-git-tag ()
  "Pre-Format git-tag. In a buffer with the output of \"git log\" up
to the last tag remove no relevant information and bad indentation."
  (interactive)
  (beginning-of-buffer)
  (flush-lines "^\\(commit\\|Author\\|Date\\)")
  (replace-regexp "^    " "" nil (point-min) (point-max))
  (replace-regexp "\n\n\n" "\n\n" nil (point-min) (point-max)))

(defun ta-prepare-commit-msg-toggle ()
  "Set the git hook \"prepare-commit-msg\".

If it already exist (ie: not ends with \".sample\") bypass it. It is mandatory when
you rebase/rewrite your git history.
If it doesn't exist, create it with the following content:
\"
#!/bin/bash
COMMIT_MSG_FILEPATH=$1
HINT=`cat $COMMIT_MSG_FILEPATH`

echo \"Subject line\" > $COMMIT_MSG_FILEPATH
echo \"\" >> $COMMIT_MSG_FILEPATH
for cached_file in `git diff --cached --name-only | sed 's/ /\n/g'`;do
    echo \"* $cached_file:\" >> $COMMIT_MSG_FILEPATH;
done
echo \"$HINT\" >> $COMMIT_MSG_FILEPATH
\""
  (interactive)
  (when-let* ((hooks (concat (cdr (project-current)) ".git/hooks/"))
              (prepare-commit-msg (concat hooks "prepare-commit-msg")))
    (if (file-exists-p prepare-commit-msg)
        (progn (delete-file prepare-commit-msg)
               (message "\"%s\" has been removed" (file-name-nondirectory prepare-commit-msg)))
      (with-temp-file prepare-commit-msg
        (insert
         "#!/bin/bash
COMMIT_MSG_FILEPATH=$1
HINT=`cat $COMMIT_MSG_FILEPATH`

echo \"Subject line\" > $COMMIT_MSG_FILEPATH
echo \"\" >> $COMMIT_MSG_FILEPATH
for cached_file in `git diff --cached --name-only | sed 's/ /\\n/g'`;do
    echo \"* $cached_file:\" >> $COMMIT_MSG_FILEPATH;
done
echo \"$HINT\" >> $COMMIT_MSG_FILEPATH"))
			(shell-command (concat "chmod +x " prepare-commit-msg))
      (message "\"%s\" has been created" (file-name-nondirectory prepare-commit-msg)))))

(shell-command "ls")
;; shell-command-to-string
(global-set-key (kbd "C-<f1>") 'ta-prepare-commit-msg-toggle)

(provide 'setup-init)