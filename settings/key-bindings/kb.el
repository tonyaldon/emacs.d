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

(global-set-key (kbd "M-p") 'avy-goto-char)
(global-set-key (kbd "M-b") 'avy-goto-line)
(global-set-key (kbd "M-f") 'ta-avy-goto-end-of-line)




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
  ;; ("C-M-a" iy-go-to-char-backward :color blue)
  ;; ("C-M-e" iy-go-to-char :color blue)
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

;;;; insight-mode

(global-set-key (kbd "M-i") 'insight-mode)

;;;; iy-go-to-char

;; (require 'iy-go-to-char)

;; (global-set-key (kbd "C-M-a") 'iy-go-to-char-backward)
;; (global-set-key (kbd "C-M-e") 'iy-go-to-char)

;; (define-key iy-go-to-char-keymap (kbd "C-b") 'backward-char)
;; (define-key iy-go-to-char-keymap (kbd "C-f") 'forward-char)
;; (define-key iy-go-to-char-keymap (kbd "M-s") 'iy-go-to-char-isearch)
;; (define-key iy-go-to-char-keymap (kbd "M-r") 'iy-go-to-char-isearch-backward)
;; (define-key iy-go-to-char-keymap (kbd "C-w") 'iy-go-to-char-kill-region)
;; (define-key iy-go-to-char-keymap (kbd "M-w") 'iy-go-to-char-kill-ring-save)

;;;; keyboard-quit

(define-key key-translation-map (kbd "M-q") (kbd "C-g"))

;;;; miscellaneous

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

;;; Per Mode

;;;; emacs-lisp-mod-map

(define-key emacs-lisp-mode-map (kbd "C-M-i") nil)

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

(define-key magit-status-mode-map (kbd "<prior>") 'insight-scroll-down-half-window)
(define-key magit-status-mode-map (kbd "<next>") 'insight-scroll-up-half-window)
(define-key magit-log-mode-map (kbd "<prior>") 'insight-scroll-down-half-window)
(define-key magit-log-mode-map (kbd "<next>") 'insight-scroll-up-half-window)

;;;; occur-mode

(require 'replace)

(define-key occur-mode-map (kbd "M-p") 'nil)
(define-key occur-mode-map (kbd "M-n") 'nil)

;;;; text-mode

(define-key text-mode-map (kbd "C-c C-l") 'ta-magit-log-other-window)
;;; Footer

(provide 'kb)
