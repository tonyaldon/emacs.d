(require 'kb)

(setq scroll-conservatively 100)

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

(defhydra hydra-scrolling
  (
   :pre (hydra-color-pre-scrolling)
   :post (hydra-color-post)
   :hint nil)
  ("<prior>" hydra-sp-reshape/body :color blue)
  ("<next>" hydra-lines/body :color blue)
  ("." hydra-sp/body :color blue)
  ("j" hydra-org/body :color blue)
  ;; ---
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("<backspace>" scroll-down-command)
  ("SPC" scroll-up-command)
  ("p" ta-scroll-down-line)
  ("n" ta-scroll-up-line)
  ("f" scroll-left)
  ("b" scroll-right)
  ;; ---
  ("u" recenter-top-bottom)
  ("a" move-to-window-line-top-bottom)
  ;; ---
  ("e" scroll-other-window)
  ("i" scroll-other-window-down)
  ("o" ta-scroll-other-window-line)
  ("x" ta-scroll-other-window-down-line)
  ("q" nil))

(key-chord-define-global "q-" 'hydra-scrolling/body)


(provide 'kb-scrolling)
