;;; About

;; A few HANDY commands I'm using daily that suits my style of
;; programming.
;;
;; These commands are mostly used:
;;   1. to operate on lines,
;;   2. to operate on sexp,
;;   3. and to mark things.

;;; Operate on lines

(defun handy-line-kill ()
  "Kill the whole current line."
  (interactive)
  (let ((column-position (current-column)))
    (kill-whole-line)
    (move-to-column column-position)))

(defun handy-line-copy ()
  "Copy current line."
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol)))

(defun handy-line-copy-paste-below ()
  "Copy current line and past it below "
  (interactive)
  (let ((init-point (point))
        (line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (save-excursion
      (next-line)
      (beginning-of-line)
      (insert (s-concat line "\n")))))

(defun handy-line-comment ()
  "Comment or uncomment the current line.

See `comment-or-uncomment-region'."
  (interactive)
  (comment-or-uncomment-region (point-at-bol) (point-at-eol)))

(defun handy-line-add-above ()
  "Add an empty line above and move the cursor to this line."
  (interactive)
  (back-to-indentation)
  (split-line))

(defun handy-line-add-below ()
  "Add an empty line below and move the cursor to this line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;; Operate on sexps

(declare-function avy-goto-word-or-subword-1 "ext:avy")
(declare-function sp-transpose-sexp "ext:smartparens")
(declare-function sp-backward-sexp "ext:smartparens")
(declare-function sp-forward-sexp "ext:smartparens")
(declare-function sp-previous-sexp "ext:smartparens")

(defun handy-sp-touch ()
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

(defun handy-sp-drag-backward ()
  "Drag sp-sexp at point backward."
  (interactive)
  (let ((delta (- (point) (plist-get (sp-get-thing) :beg))))
    (cond
     ((equal (handy-sp-touch) 'left)
      (sp-transpose-sexp)
      (sp-backward-sexp 2))
     ((equal (handy-sp-touch) 'right)
      (sp-backward-sexp)
      (sp-transpose-sexp)
      (sp-previous-sexp))
     ((equal (handy-sp-touch) 'symbol)
      (sp-backward-sexp)
      (sp-transpose-sexp)
      (sp-backward-sexp 2)
      (forward-char delta)))))

(defun handy-sp-drag-forward ()
  "Drag next sexp to the left of the previous sexp."
  (interactive)
  (let ((delta (- (point) (plist-get (sp-get-thing) :beg))))
    (cond
     ((equal (handy-sp-touch) 'left)
      (sp-forward-sexp)
      (sp-transpose-sexp)
      (sp-backward-sexp))
     ((equal (handy-sp-touch) 'right)
      (sp-transpose-sexp))
     ((equal (handy-sp-touch) 'symbol)
      (sp-forward-sexp)
      (sp-transpose-sexp)
      (sp-backward-sexp)
      (forward-char delta)))))

(defun handy-avy-copy-past-sexp ()
  "Past sexp copied using `avy' at current cursor position."
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'avy-goto-word-or-subword-1)
      (sp-copy-sexp))
    (select-window initial-window)
    (yank)))

;;; Miscellaneous

(defun handy-cycle-spacing ()
  "Wrapper on `cycle-spacing' to call it in \"fast\" mode."
  (interactive)
  (cycle-spacing nil nil 'fast))

(defun handy-add-space ()
  "Add space at point without moving."
  (interactive)
  (insert " ")
  (goto-char (- (point) 1)))

;;; Comments

(comment ; plist-get, <, handy-sp-touch
 (plist-get (sp-get-thing) :beg)
 (plist-get (sp-get-thing t) :beg)
 (handy-sp-touch)
 (< 1 3 5) ;; t
 (< 1 3 3) ;; nil
 (global-set-key (kbd "C-<f1>") 'handy-sp-touch)
 ;; (test-1 test-3 test-2)
 ;; (test-d-e-f)
 ;; (test-a-b-c)
 ;; (test-g-h-i)
 )

;;; Footer

(provide 'handy)
