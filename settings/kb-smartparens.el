(require 'avy)
(require 'kb)
(require 'kb-lines)
(require 'smartparens-config)

(smartparens-global-mode t)
(smartparens-global-strict-mode -1)
(show-smartparens-global-mode t)

(eval-after-load 'mhtml-mode '(require 'smartparens-html))
(eval-after-load 'LaTeX '(require 'smartparens-latex))
(add-to-list 'sp-navigate-consider-sgml-tags 'mhtml-mode)

(setq sp-navigate-interactive-always-progress-point t)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
(setq sp-show-pair-from-inside nil)

;; When using smartparens in sgml-mode, there is a boring message that
;; appears when running sp-forward-sexp in an self-closing tag like
;; this <meta charset="utf-8/>".
(setq sp-message-width nil)

(defun ta-drag-sexp-to-up ()
  "Drag next sexp to the left of the previous sexp.

Work as I want with `sp-navigate-interactive-always-progress-point' set to non-nil value."
  (interactive)
  (sp-forward-sexp)
  (sp-backward-sexp)
  (sp-transpose-sexp)
  (sp-backward-sexp 2))

(defun ta-drag-sexp-to-down ()
  "Drag next sexp to the left of the previous sexp.

Work as I want with `sp-navigate-interactive-always-progress-point' set to non-nil value."
  (interactive)
  (sp-forward-sexp)
  (sp-transpose-sexp)
  (sp-backward-sexp))

(defun ta-avy-copy-sexp ()
  "Copy a selected sexp at the current point"
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'avy-goto-word-or-subword-1)
      (sp-copy-sexp))
    (select-window initial-window)
    (yank)))

(defun ta-avy-kill-sexp ()
  "Kill a selected sexp and save it in the kill ring"
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'avy-goto-word-or-subword-1)
      (sp-kill-sexp))
    (select-window initial-window)
		(yank)))

(defun ta-sp-toggle-narrow (arg)
  "Toggle between `widen' and `sp-narrow-to-sexp'."
  (interactive "p")
  (if (buffer-narrowed-p) (widen)
    (sp-narrow-to-sexp arg)))

(defhydra hydra-sp
  (
   :pre (hydra-color-pre-sp)
   :post (hydra-color-post)
   :hint nil)
  ("t" hydra-lines/body :color blue)
  ;; miscellaneous
  ("." set-mark-command)
	("M-." ta-sp-toggle-narrow)
  ("T" exchange-point-and-mark)
  ("r" join-line)
  (";" sp-comment)
  ("_" ta-add-space :color blue)
  ;; kill
  ("M-d" sp-kill-sexp)
  ("DEL" sp-backward-kill-sexp)
  ("C" sp-copy-sexp)
  ("c" ta-avy-copy-sexp :color blue)
  ("C-y" sp-clone-sexp)
  ("@" ta-avy-kill-sexp :color blue)
  ;; reshape
  ("," sp-change-enclosing :color blue)
  ("'" sp-change-inner :color blue)
  (":" sp-split-sexp)
  ("M-:" sp-join-sexp)
  (">" sp-absorb-sexp)
  ("}" sp-emit-sexp)
	("%" sp-convolute-sexp)
	("C-f" sp-forward-slurp-sexp)
  ("C-b" sp-backward-slurp-sexp)
  ("C-p" sp-add-to-previous-sexp)
  ("C-n" sp-add-to-next-sexp)
  ("M-f" sp-forward-barf-sexp)
  ("M-b" sp-backward-barf-sexp)
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
  ("u" sp-down-sexp)
  ("i" sp-up-sexp)
  ("y" sp-backward-up-sexp)
  ("x" sp-backward-down-sexp)
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("d" ta-drag-sexp-to-up)
  ("s" ta-drag-sexp-to-down)
  ;; parenthese type
  ("$" sp-show-enclosing-pair)
  ("{" sp-wrap-curly)
  ("(" sp-wrap-round)
  ("[" sp-wrap-square)
  ("M-r" sp-rewrap-sexp)
  ("]" sp-swap-enclosing-sexp)
  ;; ---
  ("g" cleanup-buffer)
  ("M--" undo)
  ("q" nil))

(global-set-key (kbd "M-i") 'hydra-sp/body)


(provide 'kb-smartparens)
