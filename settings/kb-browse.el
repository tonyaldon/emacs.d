(require 'kb)
(require 'bicycle)

(setq scroll-conservatively 100)

(declare-function ta-pop-local-mark-ring "ext:kb-mark")
(declare-function counsel-outline "ext:counsel")
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

(defhydra hydra-browse
  (:pre (hydra-color-pre-browse)
        :post (hydra-color-post)
        :hint nil)
  ("t" hydra-lines/body :color blue)
  ("M-l" ta-pop-local-mark-ring)
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
  ("]" bicycle-cycle-global)
  ("TAB" bicycle-cycle)
  ("p" outline-previous-visible-heading)
  ("n" outline-next-visible-heading)
  ("M-s" counsel-outline)
  ("q" nil))

(key-chord-define-global "dp" 'hydra-browse/body)

(provide 'kb-browse)
