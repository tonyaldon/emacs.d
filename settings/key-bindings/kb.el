;;; Global key bindings settings
;;;; Packages

(require 'avy)
(require 'hydra)
(require 'iso-transl)
(require 'key-chord)

;;;; avy

(setq avy-highlight-first t)
(setq avy-style 'at-full)
(setq avy-keys (listify-key-sequence "auieyxowbnfpktsrqdljmcgh()[]<>,;.:"))

;;;; hydra

(setq-default hydra-hint-display-type  'message)

;;;; key-chord

(key-chord-mode t)
(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.2)

;;; Global key bindings
;;;; avy

(require 'avy)

(defun ta-avy-goto-end-of-line ()
  "Call `avy-goto-char' with \"\n\" as argument."
  (interactive)
  (avy-goto-char ?\n))

(global-set-key (kbd "M-)") 'ta-avy-goto-end-of-line)
(global-set-key (kbd "M-(") 'avy-goto-line)
(global-set-key (kbd "<down>") 'avy-goto-char)

;;;; buffers

(global-set-key [escape] 'kill-this-buffer)
(global-set-key (kbd "<f6>") 'save-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<right>") 'next-buffer)
(key-chord-define-global "::" 'ibuffer)

;;;; completion

(global-set-key (kbd "M-<") 'counsel-quick-access)
(global-set-key (kbd "M-e") 'counsel-find-file)
(global-set-key (kbd "C-a") 'ivy-switch-buffer)

(global-set-key (kbd "C-M-n") 'counsel-rg)

(global-set-key (kbd "C-x C-e") 'ta-fzf-emacs-settings)
(global-set-key (kbd "C-e") 'ta-fzf-dwim)


;;;; dired-mode-map

(define-key dired-mode-map (kbd "C-M-p") nil)
(define-key dired-mode-map (kbd "C-M-n") nil)

(define-key dired-mode-map (kbd ",") 'dired-mark)
(define-key dired-mode-map (kbd "t") 'dired-toggle-marks)
(define-key dired-mode-map (kbd "u") 'dired-unmark)
(define-key dired-mode-map (kbd "k") 'dired-do-kill-lines)
(define-prefix-command 'ta-dired-mark-map)
(define-key ta-dired-mark-map (kbd "r") 'dired-mark-files-regexp)
(define-key ta-dired-mark-map (kbd "e") 'dired-mark-extension)
(define-key ta-dired-mark-map (kbd "d") 'dired-mark-directories)
(define-key ta-dired-mark-map (kbd "u") 'dired-unmark-all-marks)
(define-key dired-mode-map (kbd "]") 'ta-dired-mark-map)

(define-key dired-mode-map (kbd "v") 'dired-view-file)
(define-key dired-mode-map (kbd "y") 'dired-show-file-type)
(define-key dired-mode-map (kbd "!") 'dired-do-shell-command)
(define-key dired-mode-map (kbd "&") 'dired-do-async-shell-command)
(define-key dired-mode-map (kbd "=") 'dired-diff)
(define-key dired-mode-map (kbd "g") 'revert-buffer)
(define-key dired-mode-map (kbd "L") 'dired-do-load)

(define-key dired-mode-map (kbd "i") 'dired-up-directory)
(define-key dired-mode-map (kbd "p") 'dired-previous-line)
(define-key dired-mode-map (kbd "n") 'dired-next-line)
(define-key dired-mode-map (kbd "M-)") 'dired-next-marked-file)
(define-key dired-mode-map (kbd "M-(") 'dired-next-marked-file)

(define-key dired-mode-map (kbd "+") 'dired-create-directory)
(define-key dired-mode-map (kbd "r") 'dired-do-rename)
(define-key dired-mode-map (kbd "c") 'dired-do-copy)
(define-key dired-mode-map (kbd "x") 'dired-do-flagged-delete)
(define-key dired-mode-map (kbd "w") 'dired-copy-filename-as-kill)
(define-key dired-mode-map (kbd "S") 'dired-do-symlink)
(define-key dired-mode-map (kbd "R") 'dired-do-relsymlink)

(define-prefix-command 'ta-dired-find-file-map)
(define-key ta-dired-find-file-map (kbd "e") 'ace-hacks-dired-find-file)
(define-key ta-dired-find-file-map (kbd "p") 'ace-hacks-dired-find-file-split-up)
(define-key ta-dired-find-file-map (kbd "n") 'ace-hacks-dired-find-file-split-down)
(define-key ta-dired-find-file-map (kbd "b") 'ace-hacks-dired-find-file-split-left)
(define-key ta-dired-find-file-map (kbd "f") 'ace-hacks-dired-find-file-split-right)
(define-key ta-dired-find-file-map (kbd "'") 'dired-do-find-marked-files)
(define-key dired-mode-map (kbd "e") 'ta-dired-find-file-map)

(define-key dired-mode-map (kbd "M-s") 'isearch-forward)
(define-key dired-mode-map (kbd "M-r") 'isearch-backward)
(define-key dired-mode-map (kbd "(") 'avy-goto-line)

;;;; describe

(defun ta-describe-thing-at-point ()
  "Display the full documentation of the `thing-at-point'

that is either a function or a variable.
Return nil if the symbol of the `thing-at-point' is neither a function
nor a variable."
  (interactive)
  (let ((current-symbol (symbol-at-point)))
    (cond
     ((not current-symbol))
     ((boundp current-symbol) (describe-variable current-symbol))
     ((fboundp current-symbol) (describe-function current-symbol))
     (t (message "The symbol-at-point is neither a variable or a function")))))

(defun ta-mouse-describe-thing-at-point ()
  "Call `ta-describe-thing-at-point' at cursor position."
  (interactive)
  (call-interactively 'mouse-set-point)
  (call-interactively 'ta-describe-thing-at-point))

(global-set-key (kbd "C-d") 'ta-describe-thing-at-point)
(global-set-key (kbd "<C-down-mouse-3>") 'ta-mouse-describe-thing-at-point)

;;;; handy

(require 'handy)

;;;; Operate on lines

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
  ("C-M-a" iy-go-to-char-backward :color blue)
  ("C-M-e" iy-go-to-char :color blue)
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

;;;; Operate on sexps

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

;;;; transient key bindings interface

;;;;; transient interface

(declare-function transient-define-prefix "ext:transient")
(declare-function transient-define-suffix "ext:transient")

(defmacro ta-transient-define-suffix (command)
  "Create a command COMMAND--transient that is a transient suffix command
that call interactively COMMAND."
  (let ((func-name (concat (symbol-name command) "--transient")))
    `(transient-define-suffix ,(intern func-name) ()
       (interactive)
       (call-interactively (quote ,command)))))

;;;;; describe

(declare-function yas-describe-tables "ext:yasnippet")
(declare-function yas-new-snippet "ext:yasnippet")

(ta-transient-define-suffix describe-key)
(ta-transient-define-suffix describe-keymap)
(ta-transient-define-suffix describe-function)
(ta-transient-define-suffix describe-variable)
(ta-transient-define-suffix describe-mode)

;;;;; linux

(require 'linux)

(ta-transient-define-suffix linux-switch-keyboard-layout)
(ta-transient-define-suffix linux-turn-off-laptop-output)
(ta-transient-define-suffix linux-toggle-dpi)
(ta-transient-define-suffix linux-toggle-git-commit-msg)

;;;;; miscellaneous

(ta-transient-define-suffix yas-describe-tables)
(ta-transient-define-suffix yas-new-snippet)
(ta-transient-define-suffix yas-visit-snippet-file)

(ta-transient-define-suffix apropos)
(ta-transient-define-suffix info)
(ta-transient-define-suffix image-toggle-display)
(ta-transient-define-suffix display-line-numbers-mode)

;;;;; ta-remind-me

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

;;;; eval

(global-set-key (kbd "<f1>") 'eval-defun)
(global-set-key (kbd "<f2>") 'eval-last-sexp)

;;;; insight-mode

(global-set-key (kbd "M-i") 'insight-mode)

;;;; iy-go-to-char

(require 'iy-go-to-char)

(global-set-key (kbd "C-M-a") 'iy-go-to-char-backward)
(global-set-key (kbd "C-M-e") 'iy-go-to-char)

(define-key iy-go-to-char-keymap (kbd "C-b") 'backward-char)
(define-key iy-go-to-char-keymap (kbd "C-f") 'forward-char)
(define-key iy-go-to-char-keymap (kbd "M-s") 'iy-go-to-char-isearch)
(define-key iy-go-to-char-keymap (kbd "M-r") 'iy-go-to-char-isearch-backward)
(define-key iy-go-to-char-keymap (kbd "C-w") 'iy-go-to-char-kill-region)
(define-key iy-go-to-char-keymap (kbd "M-w") 'iy-go-to-char-kill-ring-save)

;;;; keyboard-quit

(define-key key-translation-map (kbd "M-q") (kbd "C-g"))

;;;; miscellaneous

(global-set-key (kbd "<f5>") 'ta-find-file-notes)
(key-chord-define-global ";;" 'counsel-M-x)
(global-set-key (kbd "C-<") 'delete-char)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
(global-set-key (kbd "M-l") 'recenter-top-bottom)
(global-set-key (kbd "C-v") 'visual-line-mode)
(global-set-key (kbd "<f3>") 'yank)
(global-set-key (kbd "<C-escape>") 'repeat)


(global-set-key (kbd "C-s") 'linux-switch-keyboard-layout)
(global-set-key (kbd "C-c f") 'ta-copy-buffer-file-name)

;;;; rg

(global-set-key (kbd "C-r") 'rg-dwim)
(global-set-key (kbd "C-M-p") 'ta-rg-ask)

;;;; refactor

(require 'refactor)

(defhydra hydra-refactor
  (:hint nil)
  ("M-t" refactor-write-mode :color blue)
  ("a" refactor-abort-changes :color blue)
  ("e" refactor-exit :color blue)
  ("f" refactor-finish-edit :color blue)
  ("M--" undo)
  ("q" nil))

(global-set-key (kbd "M-t") 'hydra-refactor/body)

;;;; undo

(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "M-+") 'undo-redo)

;;;; zap-up-to-char

(defun ta-zap-back-to-char (char)
  "As `zap-to-char' but backward."
  (interactive (list (read-char-from-minibuffer
                      "Zap back to char: " nil 'read-char-history)))
  (zap-to-char -1 char))

(global-set-key (kbd "M-m") 'ta-zap-back-to-char)

;;; Per Mode
;;;; clojure & cider

(require 'cider)
(require 'clojure-mode)

;; cider-start-map  [[/home/tony/work/settings/emacs.d/.emacs.d/.cask/28.0/elpa/cider-20200903.1034/cider.el::960]]
;; cider-eval-commands-map [[/home/tony/work/settings/emacs.d/.emacs.d/.cask/28.0/elpa/cider-20200903.1034/cider-eval.el::1168]]

;; (define-key cider-mode-map (kbd "<f2>") 'cider-eval-last-sexp)
(define-key cider-mode-map (kbd "<f1>") 'cider-eval-defun-at-point)
(define-key cider-mode-map (kbd "<f2>") 'cider-eval-last-sexp-to-repl)
(define-key cider-mode-map (kbd "<f2>") 'cider-eval-last-sexp-to-repl)

(define-key clojure-mode-map (kbd "C-<f1>") 'cider-jack-in-clj)
;; (define-key cider-mode-map (kbd "<f2>") 'cider-load-buffer-and-switch-to-repl-buffer)
;; (define-key cider-mode-map (kbd "<f2>") 'cider-load-buffer)

(define-key cider-mode-map (kbd "C-M-i") nil)
(define-key cider-mode-map  (kbd "C-x C-e") nil)

(define-key cider-repl-mode-map  (kbd "C-x C-e") nil)
(define-key cider-repl-mode-map (kbd "M-n") nil)
(define-key cider-repl-mode-map (kbd "M-p") nil)
(define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
(define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)

;;;; emacs-lisp-mod-map

(define-key emacs-lisp-mode-map (kbd "C-M-i") nil)

;;;; company-mode-map

(require 'company)

(define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
(define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)
(define-key company-active-map (kbd "<prior>") 'company-previous-page)
(define-key company-active-map (kbd "<next>") 'company-next-page)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)

(define-key company-active-map (kbd "M-t") 'company-filter-candidates)
(define-key company-active-map (kbd "M-q") 'company-abort)

;;;; grep-mode-map, rg-mode-map

(require 'grep)
(require 'rg)


(define-key grep-mode-map (kbd "M-p") 'windmove-up)
(define-key grep-mode-map (kbd "M-n") 'windmove-down)

(define-key rg-mode-map (kbd "C-p") 'rg-prev-file)
(define-key rg-mode-map (kbd "C-n") 'rg-next-file)

;;;; ibuffer-mode-map

(define-key ibuffer-mode-map (kbd "M-p") nil)
(define-key ibuffer-mode-map (kbd "M-n") nil)
(define-key ibuffer-mode-map (kbd "M-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)

(define-key ibuffer-mode-map (kbd ",") 'ibuffer-toggle-sorting-mode)
(define-key ibuffer-mode-map (kbd "s i") 'ibuffer-invert-sorting)
(define-key ibuffer-mode-map (kbd "s a") 'ibuffer-do-sort-by-alphabetic)
(define-key ibuffer-mode-map (kbd "s v") 'ibuffer-do-sort-by-recency)
(define-key ibuffer-mode-map (kbd "s s") 'ibuffer-do-sort-by-size)
(define-key ibuffer-mode-map (kbd "s f") 'ibuffer-do-sort-by-filename/process)
(define-key ibuffer-mode-map (kbd "s m") 'ibuffer-do-sort-by-major-mode)
(define-key ibuffer-mode-map (kbd "s .") 'ibuffer-do-sort-by-vc-status)
(define-key ibuffer-mode-map (kbd "s d") 'ibuffer-do-sort-by-alphabetic-directory-first)

(define-prefix-command 'ta-ibuffer-visite-buffer-map)
(define-key ta-ibuffer-visite-buffer-map (kbd "e") 'ace-hacks-ibuffer-visit-buffer)
(define-key ta-ibuffer-visite-buffer-map (kbd "p") 'ace-hacks-ibuffer-visit-buffer-split-up)
(define-key ta-ibuffer-visite-buffer-map (kbd "n") 'ace-hacks-ibuffer-visit-buffer-split-down)
(define-key ta-ibuffer-visite-buffer-map (kbd "b") 'ace-hacks-ibuffer-visit-buffer-split-left)
(define-key ta-ibuffer-visite-buffer-map (kbd "f") 'ace-hacks-ibuffer-visit-buffer-split-right)

(define-key ibuffer-mode-map (kbd "e") 'ta-ibuffer-visite-buffer-map)
(define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer)

(define-key ibuffer-mode-map (kbd "d") 'ibuffer-mark-for-delete)
(define-key ibuffer-mode-map (kbd "C-d") 'ibuffer-mark-for-delete-backwards)
(define-key ibuffer-mode-map (kbd "x") 'ibuffer-do-kill-on-deletion-marks)
(define-key ibuffer-mode-map (kbd "U") 'ibuffer-unmark-all-marks)
(define-key ibuffer-mode-map (kbd "m") 'ibuffer-mark-forward)
(define-key ibuffer-mode-map (kbd "t") 'ibuffer-toggle-marks)
(define-key ibuffer-mode-map (kbd "u") 'ibuffer-unmark-forward)

(define-key ibuffer-mode-map (kbd "n") 'ibuffer-forward-line)
(define-key ibuffer-mode-map (kbd "p") 'ibuffer-backward-line)
(define-key ibuffer-mode-map (kbd "g") 'ibuffer-update)
(define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-backward-filter-group)
(define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-forward-filter-group)

(define-key ibuffer-mode-map (kbd "TAB") 'ibuffer-toggle-filter-group)
(define-key ibuffer-mode-map (kbd "C-b") 'ta-ibuffer-toggle-show-buffers-with-predicates)
(define-key ibuffer-mode-map (kbd "SPC") 'ta-ibuffer-switch-filter-groups)

(define-key ibuffer-mode-map (kbd "M-s") 'isearch-forward)

;;;; Info-mode-map and help-mode-map

(define-key Info-mode-map (kbd "d") 'insight-scroll-down-half-window)
(define-key Info-mode-map (kbd "s") 'insight-scroll-up-half-window)
(define-key help-mode-map (kbd "d") 'insight-scroll-down-half-window)
(define-key help-mode-map (kbd "s") 'insight-scroll-up-half-window)

;;;; isearch-mode

(require 'isearch)

(defun ta-isearch-yank-sexp-at-point ()
  "Pull sexp at point into search string."
  (interactive)
  (isearch-yank-string (thing-at-point 'sexp)))

(defun ta-isearch-yank-word-at-point ()
  "Pull word at point into search string."
  (interactive)
  (isearch-yank-string (thing-at-point 'word)))

(global-set-key (kbd "M-s") 'isearch-forward)
(global-set-key (kbd "M-r") 'isearch-backward)

(define-key isearch-mode-map (kbd "M-s") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-r") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<prior>") 'isearch-beginning-of-buffer)
(define-key isearch-mode-map (kbd "<next>") 'isearch-end-of-buffer)
(define-key isearch-mode-map (kbd "M-o") 'isearch-occur)

(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "M-.") 'ta-isearch-yank-sexp-at-point)
(define-key isearch-mode-map (kbd "M-i") 'ta-isearch-yank-word-at-point)

(define-key isearch-mode-map (kbd "M-c") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-r") 'isearch-toggle-regexp)
(define-key isearch-mode-map (kbd "M-e") 'isearch-edit-string)

(define-key isearch-mode-map (kbd "M-t") 'swiper-isearch-toggle)


;;;; ivy-minibuffer-map and swiper-map

(require 'ivy)
(require 'swiper)

(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-previous-history-element)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "<down>") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-e") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "M-a") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "M-p") 'ivy-reverse-i-search)

(define-key ivy-minibuffer-map (kbd "C-e") 'ace-hacks-ivy-visit)
(define-key ivy-minibuffer-map (kbd "C-p") 'ace-hacks-ivy-visit-split-up)
(define-key ivy-minibuffer-map (kbd "C-n") 'ace-hacks-ivy-visit-split-down)
(define-key ivy-minibuffer-map (kbd "C-b") 'ace-hacks-ivy-visit-split-left)
(define-key ivy-minibuffer-map (kbd "C-f") 'ace-hacks-ivy-visit-split-right)
(define-key ivy-minibuffer-map (kbd "C-a") 'ta-ivy-switch-to-buffer)

(define-key ivy-minibuffer-map (kbd "C-M-n") 'ta-counsel-rg-ivy-command)

(define-key swiper-map (kbd "M-q") 'minibuffer-keyboard-quit)
(define-key swiper-map (kbd "M-c") 'swiper-mc)
(define-key swiper-map (kbd "M-t") 'swiper-isearch-toggle)



;;;; magit

(require 'magit)

(define-key magit-section-mode-map (kbd "C-i") 'magit-section-toggle)
(define-key magit-section-mode-map [C-tab]     'magit-section-cycle)
(define-key magit-section-mode-map (kbd "C-SPC") 'magit-section-cycle-global)
(define-key magit-section-mode-map (kbd   "p") 'magit-section-backward)
(define-key magit-section-mode-map (kbd   "n") 'magit-section-forward)
(define-key magit-section-mode-map (kbd "M-p") 'magit-section-backward-sibling)
(define-key magit-section-mode-map (kbd "M-n") 'magit-section-forward-sibling)
(define-key magit-mode-map (kbd "i") 'magit-section-up)

(define-key magit-status-mode-map (kbd "C-M-p") 'windmove-up)
(define-key magit-status-mode-map (kbd "C-M-n") 'windmove-down)
(define-key magit-status-mode-map (kbd "<prior>") 'insight-scroll-down-half-window)
(define-key magit-status-mode-map (kbd "<next>") 'insight-scroll-up-half-window)
(define-key magit-log-mode-map (kbd "C-M-p") 'windmove-up)
(define-key magit-log-mode-map (kbd "C-M-n") 'windmove-down)
(define-key magit-log-mode-map (kbd "<prior>") 'insight-scroll-down-half-window)
(define-key magit-log-mode-map (kbd "<next>") 'insight-scroll-up-half-window)


;;;; markdown-mode

(require 'markdown-mode)

(define-key markdown-mode-map (kbd "C-M-i") nil)

;;;; occur-mode

(require 'replace)

(define-key occur-mode-map (kbd "M-p") 'nil)
(define-key occur-mode-map (kbd "M-n") 'nil)

;;;; org-mode-map
;; Note that `org' package must be loaded before using org-mode-map

(define-key org-mode-map (kbd "C-e") nil)
(define-key org-mode-map (kbd "M-e") nil)
(define-key org-mode-map (kbd "C-a") nil)
(define-key org-mode-map (kbd "M-a") nil)

(define-key org-mode-map (kbd "M-m") 'ta-org-table-previous-row)
(define-key org-mode-map (kbd "C-<tab>") 'org-shifttab)
(define-key org-mode-map (kbd "<M-return>") 'ta-org-meta-return)
(define-key org-mode-map (kbd "M-S-<down>") 'ta-org-shiftmetadown)
(define-key org-mode-map (kbd "C-t") 'org-toggle-inline-images)

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



;;;; text-mode

(define-key text-mode-map (kbd "C-c C-l") 'ta-magit-log-other-window)
;;; Footer

(provide 'kb)
