(require 'kb)
(require 'adaptive-wrap)
(require 'drag-stuff)
(load "~/.emacs.d/.cask/28.0/elpa/drag-stuff-20161108.749/drag-stuff.el")

(defvar hydra-lines-active nil)

(setq-default truncate-lines t)

(defun ta-cycle-spacing ()
  "Wrapper on `cycle-spacing' to call it in \"fast\" mode."
  (interactive)
  (cycle-spacing nil nil 'fast))

(defun ta-copy-line-below ()
  "Copy current line and past it below "
  (interactive)
  (let ((init-point (point))
        (line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (save-excursion
      (next-line)
      (beginning-of-line)
      (insert (s-concat line "\n")))))

(defun ta-copy-line ()
  "Copy current line."
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol)))

(defun ta-comment-line ()
  (interactive)
  (comment-or-uncomment-region (point-at-bol) (point-at-eol)))

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
  (:pre (progn (if insight-mode (insight-mode -1))
               (set-cursor-color "#fa87ce"))
   :post (progn (set-cursor-color "#26f9ad")
                (hydra-lines-active))
   :hint nil)
  ("M-l" recenter-top-bottom)
  ("t" hydra-sp/body :color blue)
  (";" ta-comment-line)
  ("DEL" delete-backward-char)
  ("." set-mark-command)
  ("m" exchange-point-and-mark)
  ;; action on line(s)
  ("!" flush-lines)
  ("?" keep-lines)
  ;; current line
  ("k" kill-line)
  ("l" (kill-line 0))
  ("x" ta-kill-whole-line)
  ("y" ta-copy-line-below)
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
  ("M--" undo)
  ("q" nil))

(defadvice move-beginning-of-line (before move-beginning-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice move-end-of-line (before move-end-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice hydra-lines/body (before hydra-lines-advice activate)
  (hydra-lines-active))

(key-chord-define-global "ld" 'hydra-lines/body)


(provide 'kb-lines)
