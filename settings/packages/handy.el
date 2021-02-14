;;; About

;; A few HANDY commands I'm using daily that suits my style of
;; programming.
;;
;; These commands are mostly used:
;;   1. to operate on lines,
;;   2. to operate on sexp,
;;   3. and to mark things.

;;; Declare functions

(declare-function avy-goto-word-or-subword-1 "ext:avy")

(declare-function sp-transpose-sexp "ext:smartparens")
(declare-function sp-backward-sexp "ext:smartparens")
(declare-function sp-forward-sexp "ext:smartparens")
(declare-function sp-previous-sexp "ext:smartparens")
(declare-function sp-get-thing "ext:smartparens")
(declare-function sp-get "ext:smartparens")
(declare-function sp-backward-up-sexp "ext:smartparens")
(declare-function sp-backward-down-sexp "ext:smartparens")
(declare-function sp-mark-sexp "ext:smartparens")
(declare-function sp-down-sexp "ext:smartparens")

(declare-function er/expand-region "ext:expand-region")
(declare-function er/mark-word "ext:expand-region")
(declare-function er/mark-sentence "ext:expand-region")
(declare-function er/mark-url "ext:expand-region")
(declare-function er--point-inside-string-p "ext:expand-region")
(declare-function er/mark-inside-quotes "ext:expand-region")

(declare-function org-table-beginning-of-field "org")
(declare-function org-at-table-p "ext:smartparens")

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

;;; Mark things

(defun handy-mark-pop-local ()
  (interactive)
  (set-mark-command t))

(defun handy-point-in-string-p (pt)
  "Returns t if PT is in a string"
  (eq 'string (syntax-ppss-context (syntax-ppss pt))))

(defun handy-point-in-url-p ()
  "Return the url at `point' if `point' is in an url."
  (thing-at-point 'url))

(defun handy-point-at-beginning-of-sexp-delimited-by-pairs-p ()
  "Return t if point is at beginning of sexp delimited by pairs."
  (let ((sexp (sp-get-thing)))
    (and (eq (point) (sp-get sexp :beg))
         (not (string-empty-p (sp-get sexp :op))))))

(defun handy-mark-goto-beginning-of-string (pt)
  "Go to begining of the string if PT is inside a string.

Return nil if PT isn't inside a string.
See the function `handy-point-in-string-p'"
  (if (handy-point-in-string-p pt)
      (goto-char (nth 8 (syntax-ppss pt)))
    nil))

(defun handy-mark-line (arg)
  "Mark the current line.

If call with `universal-argument', copy the line."
  (interactive "p")
  (if (equal arg 4)
      (handy-line-copy)
    ;; HACK: Have to use both `push-mark' and `set-mark' in this order
    ;;       to get expected result.
    (end-of-line)
    (push-mark (point))
    (set-mark (point))
    (beginning-of-line)))

(defun handy-mark-sexp-at-point ()
  "Mark the `sexp' at point."
  (let ((sexp-beg (beginning-of-thing 'sexp))
        (sexp-end (end-of-thing 'sexp)))
    (goto-char sexp-end)
    ;; HACK: Have to use both `push-mark' and `set-mark' in this order to
    ;;       expected result.
    (push-mark sexp-end)
    (set-mark sexp-end)
    (goto-char sexp-beg)))

(defun handy-mark-inside-pairs ()
  "An other way to do `er/mark-inside-pairs' but work for sgml-tag too."
  (interactive)
  (handy-mark-goto-beginning-of-string (point))
  ;; todo: do thing when inside a tag <tag name="tony"> (maybe use the function sgml-begining-of-tag)
  (sp-backward-up-sexp)
  (sp-mark-sexp)
  (sp-down-sexp)
  (exchange-point-and-mark)
  (sp-backward-down-sexp)
  (exchange-point-and-mark))

(defun handy-mark-inside-field ()
  "Mark current field inside org-table."
  (interactive)
  (when (org-at-table-p)
    (push-mark (point))
    (re-search-forward "|")
    (backward-char)
    (skip-chars-backward " ")
    (push-mark (point))
    (set-mark (point))
    (org-table-beginning-of-field 1)))

;;;; dwim

(defun handy-expand-region-dwim (arg)
  "If region is active, call `er/expand-region'.  If not call `er/mark-word'."
  (interactive "p")
  (if (or (region-active-p) (equal last-command this-command))
      (er/expand-region arg)
    (er/mark-word)
    (if (equal arg 4) (exchange-point-and-mark))))

(defun handy-mark-dwim (arg)
  "Mark the url, sexp or sentence at point.

If point is in a url, call `er/mark-url'.  If not mark sexp at point.
If call 2 times consecutively, call `er/mark-sentence'."
  (interactive "p")
  (cond
   ((equal last-command this-command)
    (er/mark-sentence))
   ((handy-point-in-url-p)
    (er/mark-url))
   ((or (handy-point-at-beginning-of-sexp-delimited-by-pairs-p)
        (eq (following-char) ?<))
    (sp-mark-sexp))
   ((eq (preceding-char) ?\")
    (sp-backward-sexp)
    (sp-mark-sexp))
   ((and (memq (following-char) '(32 ?\) ?\] ?\} ?>))
         (looking-back "[[:alnum:]]" 1))
    (backward-char 1)
    (handy-mark-sexp-at-point))
   (t (handy-mark-sexp-at-point)))
  (if (equal arg 4) (exchange-point-and-mark)))

(defun handy-mark-inside-dwim (&optional arg)
  "Mark things inside quotes if point is inside a string.

If not inside string, mark inside table field in `org-mode'.
In other modes, mark things inside pairs.
If call two times consecutively mark inside pairs."
  (interactive)
  (cond ((equal last-command this-command)
         (call-interactively 'handy-mark-inside-pairs))
        ((er--point-inside-string-p)
         (call-interactively 'er/mark-inside-quotes))
        ((and (equal major-mode 'org-mode) (org-at-table-p))
         (handy-mark-inside-field))
        (t (call-interactively 'handy-mark-inside-pairs))))

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

(comment ; thing-at-point, handy-point-in-url-p
 (with-temp-buffer
   (insert "https://tonyaldon.com and ...")
   (search-backward "tony")
   (handy-point-in-url-p))
 (thing-at-point 'sentence)
 (thing-at-point 'word)
 (thing-at-point 'url)
 )

;;; Footer

(provide 'handy)
