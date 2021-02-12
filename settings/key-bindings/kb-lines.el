(require 'kb)
(require 'avy)
(require 'adaptive-wrap)
(require 'drag-stuff)
(load "~/.emacs.d/.cask/28.0/elpa/drag-stuff-20161108.749/drag-stuff.el")

(defvar hydra-lines-active nil)

(setq-default truncate-lines t)

(declare-function 'ta-jsx-comment-or-uncomment-line "ext:setup-js-mode")

(defun ta-cycle-spacing ()
  "Wrapper on `cycle-spacing' to call it in \"fast\" mode."
  (interactive)
  (cycle-spacing nil nil 'fast))

(defun ta-adaptative-wrap ()
  "respect indentation with visual-mode-line"
  (adaptive-wrap-prefix-mode))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save.
see: https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el#L144-166"
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun ta-avy-kill-yank-whole-line ()
  "Call `avy-kill-whole-line' and yank it at point."
  (interactive)
  (call-interactively 'avy-kill-whole-line)
  (beginning-of-line)
  (open-line 1)
  (yank)
  (delete-backward-char 1)
  (beginning-of-line))

(defun ta-yank-line-below ()
  "copy current line and yank it to the next line.
Cursor doesn't move."
  (interactive)
  (setq init-point (point))
  (save-excursion
    (beginning-of-line)
    (setq beg-point (point))
    (end-of-line)
    (setq end-point (point))
    (setq line-text (delete-and-extract-region end-point beg-point))
    (insert line-text)
    (newline)
    (insert line-text))
  (goto-char init-point))

(defun ta-kill-ring-save-current-line ()
  "Save the current line as if killed, but don't kill it."
  (interactive)
  (setq init-point (point))
  (save-excursion
    (beginning-of-line)
    (setq beg-point (point))
    (end-of-line)
    (setq end-point (point))
    (copy-region-as-kill beg-point end-point))
  (goto-char init-point))

(defun ta-comment-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (setq end (point))
      (comment-or-uncomment-region beg end))))

(defun ta-above-new-indent ()
  "In the current line, back to indent then split line as `split-line'"
  (interactive)
  (back-to-indentation)
  (split-line))

(defun ta-below-new-indent ()
  "Do `end-of-visual-line' then `newline-and-indent'"
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun ta-add-space ()
  "Add space at point without moving."
  (interactive)
  (insert " ")
  (goto-char (- (point) 1)))

(defun ta-avy-mark-region (arg)
  "Select two lines and mark the region between them"
  (interactive "P")
  (avy-with avy-kill-ring-save-region
    (let* ((beg (save-selected-window
                  (list (avy--line arg) (selected-window))))
           (end (list (avy--line arg) (selected-window))))
      (cond
       ((not (numberp (car beg)))
        (user-error "Fail to select the beginning of region"))
       ((not (numberp (car end)))
        (user-error "Fail to select the end of region"))
       ((not (equal (cdr beg) (cdr end)))
        (user-error "Selected points are not in the same window"))
       ((< (car beg) (car end))
        (set-mark (car beg))
        (goto-char (car end))
        (end-of-line))
       (t
        (set-mark (car beg))
        (goto-char (car end))
        (exchange-point-and-mark)
        (end-of-line))))))

(defun ta-kill-whole-line ()
  "Kill the whole current line.

Preserve the column position of the cursor."
  (interactive)
  (let ((column-position (current-column)))
    (kill-whole-line)
    (move-to-column column-position)))

(defun hydra-lines-active ()
  "Toggle status of `hydra-lines-active'"
  (if hydra-lines-active
      (setq hydra-lines-active nil)
    (setq hydra-lines-active t)))

(defhydra hydra-lines
  (
   :pre (progn
          (if insight-mode (insight-mode -1))
          (set-cursor-color "#fa87ce"))
   :post (progn
           (set-cursor-color "#26f9ad")
           (hydra-lines-active))
   :hint nil)
  ("M-l" recenter-top-bottom)
  ;;; test on cursor color
  ("z" ta-test-cursor-color :color blue)
  ;;; test on cursor color
  ("v" hydra-browse/body :color blue)
  ("t" hydra-sp/body :color blue)
  (";" ta-comment-line)
  ("&" ta-jsx-comment-or-uncomment-line)
  ("DEL" delete-backward-char)
  ("." set-mark-command)
  ("m" exchange-point-and-mark)
  ("D" display-line-numbers-mode)
  ("L" goto-line)
  (">" zap-up-to-char)
  ("C-n" narrow-to-region)
  ;; action on line(s)
  (":" ta-avy-mark-region)
  ("'" mark-paragraph)
  ("c" avy-copy-line)
  ("@" ta-avy-kill-yank-whole-line)
  ("C" avy-copy-region)
  ("%" avy-kill-region)
  ("!" flush-lines)
  ("?" keep-lines)
  ;; current line
  ("/" ta-mark-current-line)
  (")" ta-mark-end-of-line)
  ("k" kill-line)
  ("l" (kill-line 0))
  ("x" ta-kill-whole-line)
  ("w" ta-kill-ring-save-current-line :color blue)
  ("y" ta-yank-line-below)
  ("r" join-line)
  ("o" open-line)
  ("O" delete-blank-lines)
  ("," ta-cycle-spacing)
  ;; to insert text
  ("u" ta-above-new-indent :color blue)
  ("]" ta-below-new-indent :color blue)
  ("_" ta-add-space :color blue)
  ;; quick motions
  ("n" next-logical-line)
  ("p" previous-logical-line)
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
  ("g" cleanup-buffer)
  ("M--" undo)
  ("q" nil))

(defadvice move-beginning-of-line (before move-beginning-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice move-end-of-line (before move-end-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice hydra-lines/body (before hydra-lines-advice activate)
  (hydra-lines-active))

(defadvice ta-avy-mark-region (after ta-avy-mark-region-advice activate)
  (if hydra-lines-active nil
    (hydra-lines/body)))

(add-hook 'visual-line-mode-hook 'ta-adaptative-wrap)

(key-chord-define-global "ld" 'hydra-lines/body)


(provide 'kb-lines)
