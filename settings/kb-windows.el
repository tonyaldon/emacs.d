(require 'ace-window)
(require 'framer)
(require 'kb)
(require 'transpose-frame)

(winner-mode t)

(setq aw-char-position 'top-left)
(setq aw-ignore-current nil)
(setq aw-leading-char-style 'char)
(setq aw-background nil)
(setq aw-keys '(?s ?r ?d ?l ?t))

(declare-function ta-term "ext:kb-term")

(defun enable-zoom-one-shot-keybindings ()
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "+") 'zoom-frm-in)
     (define-key map (kbd "-") 'zoom-frm-out)
     (define-key map (kbd "0") 'zoom-frm-unzoom)
     map) t))

(defun ta-drag-window-left ()
  "Drag current window one window to the left."
  (interactive)
  (aw-swap-window (window-in-direction 'left)))

(defun ta-drag-window-right ()
  "Drag current window one window to the right."
  (interactive)
  (aw-swap-window (window-in-direction 'right)))

(defun ta-drag-window-above ()
  "Drag current window one window to the above."
  (interactive)
  (aw-swap-window (window-in-direction 'above)))

(defun ta-drag-window-below ()
  "Drag current window one window to the below."
  (interactive)
  (aw-swap-window (window-in-direction 'below)))

(defun ta-split-window-right ()
  "Chain `split-window-right' and `windmove-right'."
  (interactive)
  (split-window-right)
	(recenter)
	(windmove-right)
  (recenter))

(defun ta-split-window-down ()
  "Chain `split-window-below' and `windmove-down'."
  (interactive)
	(split-window-below)
  (recenter)
	(windmove-down)
	(recenter))

(defun ta-ace-kill-buffer ()
  "Kill buffer in other window.

Other window is selected with `ace-window'."
  (interactive)
  (ace-select-window)
  (kill-this-buffer)
  (other-window))

(defhydra hydra-windows-size
  (
   :pre (hydra-color-pre-windows)
   :post (hydra-color-post)
   :hint nil)
  ("b" (shrink-window-horizontally 5))
  ("f" (enlarge-window-horizontally 5))
  ("p" (shrink-window 5))
  ("n" (enlarge-window 5))
  ("q" nil))

(defhydra hydra-windows
  (
   :pre (hydra-color-pre-windows)
   :post (hydra-color-post)
   :hint nil)
  ("p" ta-drag-window-above)
  ("n" ta-drag-window-below)
  ("b" ta-drag-window-left)
  ("f" ta-drag-window-right)
  ("/" ace-swap-window :color blue)
  ("T" transpose-frame)
  ("t" window-toggle-side-windows)
  ("i" clone-indirect-buffer-other-window :color blue)
  ("o" ta-split-window-right :color blue)
  (";" ta-split-window-down :color blue)
	("d" ace-delete-window)
  ("D" ta-ace-kill-buffer :color blue)
  ("u" winner-undo)
  ("]" winner-redo :color blue)
  (">" make-frame :color blue)
  ("s" hydra-windows-size/body :color blue)
  ("+" text-scale-adjust :color blue)
  ("." framer-push :color blue)
  ("x" framer-undo)
  (":" framer-redo)
  ("r" (ta-term "/bin/bash") :color blue)
  ;; ---
  ("c" ace-window :color blue)
  ("'" aw-flip-window :color blue)
  ("q" nil))

(defadvice windmove-do-window-select
    (before windmove-do-window-select-advice activate)
  "Push `selected-window' in the ring used by `aw-flip-window'.

The function `windmove-left', `windmove-right', `windmove-up' and
`windmove-down' are interactive wrappers to `windmove-do-window-select'."
  (aw--push-window (selected-window)))

(defadvice clone-indirect-buffer-other-window
		(after ta-clone-indirect-buffer-other-window-advice activate)
	(recenter))

(global-set-key (kbd "M-u") 'hydra-windows/body)
(global-set-key (kbd "C-<tab>") 'ace-window)

(global-set-key (kbd "M-b") 'windmove-left)
(global-set-key (kbd "M-f") 'windmove-right)
(global-set-key (kbd "M-p") 'windmove-up)
(global-set-key (kbd "M-n") 'windmove-down)
(define-key markdown-mode-map (kbd "M-b") 'windmove-left)
(define-key markdown-mode-map (kbd "M-f") 'windmove-right)
(define-key markdown-mode-map (kbd "M-p") 'windmove-up)
(define-key markdown-mode-map (kbd "M-n") 'windmove-down)
(define-key Info-mode-map (kbd "M-n") 'windmove-down)


(define-key dired-mode-map (kbd "C-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)
(global-set-key (kbd "C-o") 'delete-other-windows)
(global-set-key (kbd "M-o") 'delete-window)
(define-key term-raw-map (kbd "M-o") 'delete-window)
(define-key term-mode-map (kbd "M-o") 'delete-window)


(provide 'kb-windows)
