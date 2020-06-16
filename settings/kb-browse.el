(require 'kb)
(require 'bicycle)

(setq scroll-conservatively 100)
(setq recenter-positions '(top bottom middle))

(declare-function ta-pop-local-mark-ring "ext:kb-mark")
(declare-function counsel-outline "ext:counsel")
(declare-function ace-window "ext:ace-window")
(declare-function outline-previous-visible-heading "outline")
(declare-function outline-next-visible-heading "outline")
(declare-function org-narrow-to-subtree "org")

(defun ta-toggle-narrow ()
  "Toggle between `widen' and `org-narrow-to-subtree'."
  (interactive)
  (if (buffer-narrowed-p) (widen)
    (org-narrow-to-subtree)))

(defun ta-scroll-down-line ()
  "Scroll down of one line"
  (interactive)
  (scroll-up-line -1))

(defun ta-scroll-up-line ()
  "Scroll up of one line"
  (interactive)
  (scroll-up-line))

(defun ta-scroll-other-window-line ()
  "Scroll up of one line in other window. See `scroll-other-window'"
  (interactive)
  (scroll-other-window 1))

(defun ta-scroll-other-window-down-line ()
  "Scroll up of one line in other window. See `scroll-other-window'"
  (interactive)
  (scroll-other-window-down 1))

(defun ta-scroll-down-half-window ()
  "Scroll down of half window."
  (interactive)
  (let ((window-middle (/ (window-body-height) 2)))
    (scroll-up-line window-middle)))

(defun ta-scroll-up-half-window ()
  "Scroll up of half window."
  (interactive)
  (let ((window-middle (/ (window-body-height) 2)))
    (scroll-up-line (- 0 window-middle))))

(defun ta-outline-toggle-global ()
  "Toggle visibility of all outline (see `outline-mode') sections.

This command toggle between this following levels:
1. TREES:    Show all headings, treaing top-level code blocks
             as sections (i.e. their first line is treated as
             a heading).
2. ALL:      Show everything, except code blocks that have been
             collapsed individually (using a `hideshow' command
             or function).

This is a variant off the `bicycle-cycle-global' with two
level less."
  (interactive)
  (setq deactivate-mark t)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward outline-regexp nil t)
      (user-error "Found no heading"))
    (cond
     ((eq last-command 'outline-cycle-trees)
      (outline-show-all)
      (bicycle--message "ALL"))
     (t
      (outline-hide-sublevels (bicycle--level))
      (outline-map-region
       (lambda ()
         (when (bicycle--top-level-p)
           (outline-show-branches)))
       (point-min)
       (point-max))
      (bicycle--message "TREES")
      (setq this-command 'outline-cycle-trees)))))

(defun ta-aw-other-window-scroll-buffer ()
  "Use `ace-window' to set `other-window-scroll-buffer'."
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'ace-window)
      (setq other-window-scroll-buffer (current-buffer)))
    (select-window initial-window)))

(defun ta-aw-reset-other-window-scroll-buffer ()
  "Reset `other-window-scroll-buffer' to nil when not a displayed buffer.

Use this function to advice `scroll-other-window' and `scroll-other-window-down'
before. This prevent to popup the buffer `other-window-scroll-buffer' if it
was not being displayed."
  (when (and other-window-scroll-buffer
						 (not (get-buffer-window other-window-scroll-buffer)))
		(setq other-window-scroll-buffer nil)))

(defadvice scroll-other-window
		(before ta-aw-reset-other-window-scroll-buffer-advice activate)
	"Reset `other-window-scroll-buffer' to nil when not a displayed buffer.

This prevent to popup the buffer `other-window-scroll-buffer' if it
was not being displayed.

See `ta-aw-other-window-scroll-buffer'."
  (ta-aw-reset-other-window-scroll-buffer))

(defadvice scroll-other-window-down
		(before ta-aw-reset-other-window-scroll-buffer-advice activate)
	"Reset `other-window-scroll-buffer' to nil when not a displayed buffer.

This prevent to popup the buffer `other-window-scroll-buffer' if it
was not being displayed.

See `ta-aw-other-window-scroll-buffer'."
  (ta-aw-reset-other-window-scroll-buffer))

(defhydra hydra-browse
  (:pre (hydra-color-pre-browse)
        :post (hydra-color-post)
        :hint nil)
  ("t" hydra-lines/body :color blue)
  ("M-l" ta-pop-local-mark-ring)
  ("c" ta-aw-other-window-scroll-buffer)
	("b" beginning-of-buffer)
  ("f" end-of-buffer)
  ("<backspace>" scroll-down-command)
  ("SPC" scroll-up-command)
  ("s" ta-scroll-down-half-window)
  ("d" ta-scroll-up-half-window)
  ("<up>" ta-scroll-down-line)
  ("<down>" ta-scroll-up-line)
  (")" scroll-left)
  ("(" scroll-right)
  ("u" recenter-top-bottom)
  ("a" move-to-window-line-top-bottom)
  ("<prior>" scroll-other-window-down)
  ("<next>" scroll-other-window)
  ("o" ta-scroll-other-window-line)
  ("e" ta-scroll-other-window-down-line)
  ("." ta-toggle-narrow)
  ("]" ta-outline-toggle-global)
  ("TAB" bicycle-cycle)
  ("/" org-cycle)
  ("p" outline-previous-visible-heading)
  ("n" outline-next-visible-heading)
  ("M-s" counsel-outline)
  ("q" nil))

(key-chord-define-global "dp" 'hydra-browse/body)

(define-key Info-mode-map (kbd "d") 'ta-scroll-up-half-window)
(define-key Info-mode-map (kbd "s") 'ta-scroll-down-half-window)


(provide 'kb-browse)
