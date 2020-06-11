(require 'ring)

(defvar framer-ring-size 10)
(defvar framer-ring nil)
(defvar framer-undo-counter nil)
(defvar framer-redo-frame nil)

(defun framer-set-ring ()
	"Initialize `framer-ring' to an empty ring if not define yet."
  (unless framer-ring
    (setq framer-ring (make-ring framer-ring-size))))

(defun framer-flush ()
  "Flush `framer-ring'."
  (interactive)
	(setq framer-ring (make-ring framer-ring-size)))

(defun framer-push ()
  "Push the window's state into `framer-ring'."
  (interactive)
  (framer-set-ring)
  (ring-insert framer-ring (window-state-get)))

(defun framer-undo ()
  "Cycle through `framer-ring'."
  (interactive)
  (unless (eq last-command 'framer-undo)
    (setq framer-redo-frame (window-state-get))
    (setq framer-undo-counter 0)
    (framer-set-ring))
  (unless (ring-empty-p framer-ring)
    (window-state-put (ring-ref framer-ring framer-undo-counter))
		(cl-incf framer-undo-counter)))

(defun framer-redo ()
  "Go back to the last window's state before start to

cycling in `framer-ring' with `framer-undo'."
  (interactive)
  (if (eq last-command 'framer-undo)
      (window-state-put framer-redo-frame)))


(provide 'framer)
