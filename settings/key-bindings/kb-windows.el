(require 'ace-window)
(require 'framer)
(require 'kb)
(require 'transpose-frame)
(require 'markdown-mode)


;;; Set up variables

(winner-mode t)

(setq windmove-wrap-around t)

(setq aw-char-position 'top-left)
(setq aw-ignore-current nil)
(setq aw-leading-char-style 'char)
(setq aw-background nil)
(setq aw-keys '(?s ?r ?d ?l ?t))
(setq aw-dispatch-always t)

(declare-function ta-term "ext:kb-term")

;;; Utils: Window commands

;;;; dired-mode

(defun ta-dired-width (dir)
  "Return the number of characters of the bigger file or directory in

a dired buffer generate with DIR as `dired-directory'."
  (with-current-buffer (dired-noselect dir)
    (-max (--map (length (-last-item (s-split "/" it)))
                 (dired-utils-get-all-files)))))

(defun ta-dired-resize-window-horizontaly ()
  "In `dired-mode', resize `selected-window' to `ta-dired-width'."
  (interactive)
  (let* ((ww (window-width))
         (dired-width (ta-dired-width dired-directory))
         (delta (- ww dired-width)))
    (if (> delta 0)
        (shrink-window-horizontally (- delta 5)) ; 5 is an arbitrary margin
      (enlarge-window-horizontally (+ 5 (- delta)))))) ; 5 is an arbitrary margin

(defun ta--dired-side-by-side (current-directory)
  "Do the layout job of `ta-dired-side-by-side'."
  (delete-other-windows)
  (dired current-directory)
  (split-window-right))

(defun ta-dired-side-by-side ()
  "Pop two dired buffer side by side.

If `current-buffer' is visiting a file, the root directory of this
file becomes `dired-directory' of the popped dired buffers. If not
visiting a file, the $HOME directory is chosen to be the
`dired-directory'"
  (interactive)
  (let ((current-buffer-file-name (buffer-file-name))
        (current-dired-directory dired-directory))
    (cond
     (current-buffer-file-name
      (ta--dired-side-by-side (file-name-directory current-buffer-file-name)))
     (current-dired-directory
      (ta--dired-side-by-side current-dired-directory))
     (t
      (ta--dired-side-by-side (expand-file-name "~/"))))))

;;;; Other

(defun ta-term-bash ()
  (interactive)
  (ta-term "/bin/bash"))

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
  (:pre (set-cursor-color "#ffd500")
   :post (set-cursor-color "#26f9ad")
   :hint nil)
  ("b" (shrink-window-horizontally 5))
  ("f" (enlarge-window-horizontally 5))
  ("p" (shrink-window 5))
  ("n" (enlarge-window 5))
  ("q" nil))

(defhydra hydra-windows
  (:pre (progn
          (remove-hook 'post-command-hook 'insight-check-cursor-color)
          (set-cursor-color "#ffd500"))
   :post (progn
           (add-hook 'post-command-hook 'insight-check-cursor-color)
           (set-cursor-color "#26f9ad"))
   :hint nil)
  ("t" hydra-lines/body :color blue)
  ("C-p" ta-drag-window-above)
  ("C-n" ta-drag-window-below)
  ("C-b" ta-drag-window-left)
  ("C-f" ta-drag-window-right)
  ("/" ace-swap-window)
  ("b" windmove-left)
  ("f" windmove-right)
  ("p" windmove-up)
  ("n" windmove-down)
  ("M-t" transpose-frame)
  ("c" ace-window)
  ("'" aw-flip-window :color blue)
  ("<next>" window-toggle-side-windows)
  ("i" clone-indirect-buffer-other-window)
  ("r" ta-term-bash :color blue)
  ("l" ta-dired-side-by-side)
  ("e" ta-split-window-right)
  ("," ta-split-window-down)
  ("d" ace-delete-window)
  ("k" ta-ace-kill-buffer)
  ("u" winner-undo)
  ("]" winner-redo)
  ("." framer-push :color blue)
  ("x" framer-undo)
  (":" framer-redo)
  (">" make-frame :color blue)
  ("SPC" ta-dired-resize-window-horizontaly)
  ("s" hydra-windows-size/body :color blue)
  ("+" balance-windows)
  ("M-+" balance-windows-area)
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

;;; Side windows

(defun ta--side-window-to-window ()
  "Make selected side window the only window in the frame."
  (interactive)
  (let ((buffer (current-buffer))
        wnd-list)
    (delete-window)
    (display-buffer-at-bottom buffer nil)
    (setq wnd-list (window-list))
    (select-window
     (nth (-elem-index buffer (-map 'window-buffer wnd-list))
          wnd-list))
    (delete-other-windows)))

(defun ta-side-window-p (window)
  "Return t if WINDOW is a side window."
  (-contains?
   (-map 'car (window-parameters window))
   'window-side))

(defun ta-delete-other-windows ()
  "Delete other windows also when the `selected-window' is a side window."
  (interactive)
  (let ((side-window-p (ta-side-window-p (selected-window))))
    (if side-window-p (ta--side-window-to-window) (delete-other-windows))))

(defun ta-delete-window ()
  "Delete selected window also when other window is a side window."
  (interactive)
  (if (and (eq (length (window-list)) 2)
           (ta-side-window-p (cadr (window-list))))
      (progn
        (other-window 1)
        (ta--side-window-to-window))
    (delete-window)))

;;; Key bindings

(global-set-key (kbd "C-o") 'ta-delete-other-windows)
(global-set-key (kbd "M-o") 'ta-delete-window)

(global-set-key (kbd "C-p") 'winner-undo)
(global-set-key (kbd "C-+") 'winner-redo)

(global-set-key (kbd "M-u") 'hydra-windows/body)

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
(define-key term-raw-map (kbd "M-o") 'delete-window)
(define-key term-mode-map (kbd "M-o") 'delete-window)

(global-set-key (kbd "M-<next>") 'window-toggle-side-windows)

(provide 'kb-windows)
