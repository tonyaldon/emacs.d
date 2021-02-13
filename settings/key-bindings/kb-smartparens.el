(require 'avy)
(require 'kb)

(declare-function 'hydra-lines/body "ext:kb-lines")

(defun ta-sp-drag-backward ()
  "Drag sp-sexp at point backward."
  (interactive)
  (let ((delta (- (point) (plist-get (sp-get-thing) :beg))))
    (cond
     ((equal (ta-sp-touch) 'left)
      (sp-transpose-sexp)
      (sp-backward-sexp 2))
     ((equal (ta-sp-touch) 'right)
      (sp-backward-sexp)
      (sp-transpose-sexp)
      (sp-previous-sexp))
     ((equal (ta-sp-touch) 'symbol)
      (sp-backward-sexp)
      (sp-transpose-sexp)
      (sp-backward-sexp 2)
      (forward-char delta)))))

(defun ta-sp-drag-forward ()
  "Drag next sexp to the left of the previous sexp."
  (interactive)
  (let ((delta (- (point) (plist-get (sp-get-thing) :beg))))
    (cond
     ((equal (ta-sp-touch) 'left)
      (sp-forward-sexp)
      (sp-transpose-sexp)
      (sp-backward-sexp))
     ((equal (ta-sp-touch) 'right)
      (sp-transpose-sexp))
     ((equal (ta-sp-touch) 'symbol)
      (sp-forward-sexp)
      (sp-transpose-sexp)
      (sp-backward-sexp)
      (forward-char delta)))))

(defun ta-sp-touch ()
  "Indicate if cursor is \"touching\" the right or the left of a sp-sexp.

If the cursor touches the right of a sp-sexp, return symbol 'right.
If the cursor touches the left of a sp-sexp, return symbol 'left.
If the cursor is strickly inside a symbol 'symbol.
If the cursor doesn't touch any sp-sexp, return nil."
  (let ((beg-of-next-thing (plist-get (sp-get-thing) :beg))
        (end-of-prev-thing (plist-get (sp-get-thing t) :end)))
    (cond ((equal (point) beg-of-next-thing) 'left)
          ((equal (point) end-of-prev-thing) 'right)
          ((< end-of-prev-thing (point) beg-of-next-thing) nil)
          (t 'symbol))))

(comment ; plist-get, <, ta-sp-touch
 (plist-get (sp-get-thing) :beg)
 (plist-get (sp-get-thing t) :beg)
 (ta-sp-touch)
 (< 1 3 5) ;; t
 (< 1 3 3) ;; nil
 (global-set-key (kbd "C-<f1>") 'ta-sp-touch)
 ;; (test-1 test-3 test-2)
 ;; (test-d-e-f)
 ;; (test-a-b-c)
 ;; (test-g-h-i)
 )

(defun ta-avy-copy-past-sexp ()
  "Past sexp copied using `avy' at current cursor position."
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'avy-goto-word-or-subword-1)
      (sp-copy-sexp))
    (select-window initial-window)
    (yank)))


(defhydra hydra-sp
  (
   :pre (progn
          (if insight-mode (insight-mode -1))
          (set-cursor-color "#f92672"))
   :post (set-cursor-color "#26f9ad")
   :hint nil)
  ("v" hydra-browse/body :color blue)
  ("t" hydra-lines/body :color blue)
  ;; miscellaneous
  ("." set-mark-command)
  ("T" exchange-point-and-mark)
  ("r" join-line)
  ;; (";" sp-comment)
  ("_" ta-add-space :color blue)
  ;; kill
  ("M-d" sp-kill-sexp)
  ("DEL" sp-backward-kill-sexp)
  ("C" sp-copy-sexp)
  ("c" ta-avy-copy-past-sexp :color blue)
  ("C-y" sp-clone-sexp)
  ("@" ta-avy-kill-sexp :color blue)
  ;; reshape
  ("," sp-change-enclosing :color blue)
  (";" sp-change-inner :color blue)
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
  ("i" sp-backward-up-sexp)
  ("y" sp-up-sexp)
  ("x" sp-backward-down-sexp)
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("d" ta-sp-drag-backward)
  ("s" ta-sp-drag-forward)
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


(key-chord-define-global "-0" 'hydra-sp/body)
(key-chord-define-global "sr" 'ta-avy-copy-past-sexp)

(provide 'kb-smartparens)
