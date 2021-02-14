;;; About

;; A few HANDY commands I'm using daily that suits my style of
;; programming.
;;
;; These commands are mostly used:
;;   1. to operate on lines,
;;   2. to operate on sexp,
;;   3. and to mark things.

;;; Operate on sexp

(declare-function avy-goto-word-or-subword-1 "ext:avy")
(declare-function sp-transpose-sexp "ext:smartparens")
(declare-function sp-backward-sexp "ext:smartparens")
(declare-function sp-forward-sexp "ext:smartparens")
(declare-function sp-previous-sexp "ext:smartparens")

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

(defun ta-avy-copy-past-sexp ()
  "Past sexp copied using `avy' at current cursor position."
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'avy-goto-word-or-subword-1)
      (sp-copy-sexp))
    (select-window initial-window)
    (yank)))

;;; Comments

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

;;; Footer

(provide 'handy)
