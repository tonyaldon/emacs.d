(require 'kb)
(require 'kb-term)
(require 'ace-window)
(require 'transpose-frame)
(require 'zoom-frm)
(require 'ibuffer)

(setq aw-background nil)
(setq aw-keys '(?s ?r ?d ?l))

;; remember window's display
(winner-mode t)

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
  (windmove-right))

(defun ta-split-window-down ()
  "Chain `split-window-below' and `windmove-down'."
  (interactive)
  (split-window-below)
  (windmove-down))

(defhydra hydra-windows-size
  (
   :pre (hydra-color-pre-windows)
   :post (hydra-color-post)
   :hint nil)
  ("b" shrink-window-horizontally)
  ("f" enlarge-window-horizontally)
  ("p" enlarge-window)
  ("n" shrink-window)
  ("q" nil))

(defhydra hydra-windows
  (
   :pre (hydra-color-pre-windows)
   :post (hydra-color-post)
   :hint nil)
  ("<prior>" hydra-sp-reshape/body :color blue)
  ("<next>" hydra-lines/body :color blue)
  ("." hydra-sp/body :color blue)
  ("j" hydra-org/body :color blue)
  ("m" hydra-scrolling/body :color blue)
  ;; ---
  ("s" ta-term-bash :color blue)
  ("b" windmove-left)
  ("f" windmove-right)
  ("p" windmove-up)
  ("n" windmove-down)
  ;; ---
  ("<up>" ta-drag-window-above)
  ("<down>" ta-drag-window-below)
  ("<left>" ta-drag-window-left)
  ("<right>" ta-drag-window-right)
  ;; ---
  ("r" ta-split-window-right)
  ("d" ta-split-window-down)
  ("t" transpose-frame :exit t)
  ("TAB" ace-window)
  ("SPC" ace-swap-window)
  ("l" aw-flip-window :color blue)
  ("M-d" ace-delete-window )
  ("C-o" delete-other-windows)
  ("(" ivy-switch-buffer :color blue)
  (")" counsel-find-file :color blue)
  ("u" (progn (winner-undo) (setq this-command 'winner-undo)))
  ("x" make-frame)
  ("w" hydra-windows-size/body :color blue)
  ("i" text-scale-adjust :color blue)
  ("z" (progn (zoom-frm-in)
              (enable-zoom-one-shot-keybindings)) :color blue)
  ("q" nil))


(defadvice windmove-do-window-select
    (before windmove-do-window-select-advice activate)
	"Push `selected-window' in the ring used by `aw-flip-window'.

The function `windmove-left', `windmove-right', `windmove-up' and
`windmove-down' are interactive wrappers to `windmove-do-window-select'."
  (aw--push-window (selected-window)))

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

(define-key dired-mode-map (kbd "C-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)
(global-set-key (kbd "C-o") 'delete-other-windows)
(global-set-key (kbd "M-o") 'delete-window)
(define-key term-mode-map (kbd "M-o") 'delete-window)


(provide 'kb-windows)
