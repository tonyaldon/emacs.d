;;; Packages

(require 'ace-window)
(require 'dired)
(require 'ivy)
(require 'ibuffer)

;;; Utility functions

(defun ace-hacks-visite (buffer-or-file &optional direction)
  "Visite FILE-OR-BUFFER in DIRECTION in the `selected-window'

after having split it.
DIRECTION must be one of this keywords: :left, :right, :up, :down.
If DIRECTION is nil or omitted, visite FILE-OR-BUFFER in the `selected-window'."
  (unless (one-window-p) (call-interactively 'ace-window))
	(cond
	 ((eq direction :left)
		(split-window-right))
	 ((eq direction :right)
		(split-window-right)
		(windmove-right))
	 ((eq direction :up)
		(split-window-below)
		(recenter))
	 ((eq direction :down)
		(split-window-below)
		(recenter)
		(windmove-down))
	 (t nil))
	(if (bufferp buffer-or-file)
			(switch-to-buffer buffer-or-file)
		(find-file buffer-or-file)))

;;; Dired

(defun ace-hacks-dired-find-file ()
  "Open file at point in window selected with `ace-window'."
  (interactive)
  (when-let ((file-at-point (dired-file-name-at-point)))
		(ace-hacks-visite (expand-file-name file-at-point) nil)))

(defun ace-hacks-dired-find-file-split-left ()
  "Open file at point on the left part of the split window
selected with `ace-window'."
  (interactive)
  (when-let ((file-at-point (dired-file-name-at-point)))
		(ace-hacks-visite (expand-file-name file-at-point) :left)))

(defun ace-hacks-dired-find-file-split-right ()
  "Open file at point on the right part of the split window
selected with `ace-window'."
  (interactive)
	(when-let ((file-at-point (dired-file-name-at-point)))
		(ace-hacks-visite (expand-file-name file-at-point) :right)))

(defun ace-hacks-dired-find-file-split-up ()
  "Open file at point on the up part of the split window
selected with `ace-window'."
  (interactive)
  (when-let ((file-at-point (dired-file-name-at-point)))
		(ace-hacks-visite (expand-file-name file-at-point) :up)))

(defun ace-hacks-dired-find-file-split-down ()
  "Open file at point on the down part of the split window
selected with `ace-window'."
  (interactive)
  (when-let ((file-at-point (dired-file-name-at-point)))
		(ace-hacks-visite (expand-file-name file-at-point) :down)))

;;; Ibuffer

(defun ace-hacks-ibuffer-visit-buffer ()
  "Visit buffer at point in window selected with `ace-window'."
  (interactive)
  (when-let ((buffer-at-point (ibuffer-current-buffer t)))
		(ace-hacks-visite buffer-at-point nil)))

(defun ace-hacks-ibuffer-visit-buffer-split-left ()
  "Visit buffer at point on the left part of the split window
selected with `ace-window'."
  (interactive)
  (when-let ((buffer-at-point (ibuffer-current-buffer t)))
		(ace-hacks-visite buffer-at-point :left)))

(defun ace-hacks-ibuffer-visit-buffer-split-right ()
  "Visit buffer at point on the right part of the split window
selected with `ace-window'."
  (interactive)
	(when-let ((buffer-at-point (ibuffer-current-buffer t)))
		(ace-hacks-visite buffer-at-point :right)))

(defun ace-hacks-ibuffer-visit-buffer-split-up ()
  "Visit buffer at point on the up part of the split window
selected with `ace-window'."
  (interactive)
  (when-let ((buffer-at-point (ibuffer-current-buffer t)))
		(ace-hacks-visite buffer-at-point :up)))

(defun ace-hacks-ibuffer-visit-buffer-split-down ()
  "Visit buffer at point on the down part of the split window
selected with `ace-window'."
  (interactive)
  (when-let ((buffer-at-point (ibuffer-current-buffer t)))
		(ace-hacks-visite buffer-at-point :down)))

;;; Ivy

(defvar ace-hacks-ivy-callers-alist
  '((ivy-switch-buffer . ivy--switch-buffer-action)
		(ivy-switch-buffer-other-window . ivy--switch-buffer-action)
		(counsel-find-file . (lambda (buffer-or-file)
													 (find-file (expand-file-name buffer-or-file ivy--directory)))))
  "Alist of (CALLER . RESOLVER) used with `ivy'.

When `ivy' system completion is called with CALLER and the selected element
is ELT, the function RESOLVER is called with ELT as argument.
For instance, (ivy-switch-buffer . ivy--switch-buffer-action) is valid
association and so '((ivy-switch-buffer . ivy--switch-buffer-action)) is a valid
value for `ace-hacks-ivy-callers-alist'.")

(defun ace-hacks--ivy-visit (buffer-or-file caller)
  "Function to be used within ivy actions."
  (when-let (resolver (alist-get caller ace-hacks-ivy-callers-alist))
		(funcall resolver buffer-or-file)))

(defun ace-hacks--ivy-visit-action (buffer-or-file direction)
	"Visite FILE-OR-BUFFER in DIRECTION in the `selected-window'

after having split it.
DIRECTION must be one of this keywords: :left, :right, :up, :down.
If DIRECTION is nil or omitted, visite FILE-OR-BUFFER in the `selected-window'."
	(let ((caller (ivy-state-caller ivy-last)))
    (if (not (alist-get caller ace-hacks-ivy-callers-alist))
        (message "caller (%s) not listed in ace-hacks-ivy-callers-alist" caller)
      (unless (one-window-p) (call-interactively 'ace-window))
			(cond
			 ((eq direction :left)
				(split-window-right))
			 ((eq direction :right)
				(split-window-right)
				(windmove-right))
			 ((eq direction :up)
				(split-window-below)
				(recenter))
			 ((eq direction :down)
				(split-window-below)
				(recenter)
				(windmove-down))
			 (t nil))
			(ace-hacks--ivy-visit buffer-or-file caller))))

(defun ace-hacks-ivy-visit ()
  "Visit `ivy' selection in window selected with `ace-window'.
This command must be bind in `ivy-minibuffer-map'."
  (interactive)
  (ivy-set-action (lambda (selection)
										(ace-hacks--ivy-visit-action selection t)))
  (ivy-done))

(defun ace-hacks-ivy-visit-split-left ()
  "Visit `ivy' selection on the left part of the split window
selected with `ace-window'. 
This command must be bind in `ivy-minibuffer-map'."
  (interactive)
  (ivy-set-action (lambda (selection)
										(ace-hacks--ivy-visit-action selection :left)))
  (ivy-done))

(defun ace-hacks-ivy-visit-split-right ()
  "Visit `ivy' selection on the right part of the split window
selected with `ace-window'. 
This command must be bind in `ivy-minibuffer-map'."
  (interactive)
  (ivy-set-action (lambda (selection)
										(ace-hacks--ivy-visit-action selection :right)))
  (ivy-done))

(defun ace-hacks-ivy-visit-split-up ()
  "Visit `ivy' selection on the up part of the split window
selected with `ace-window'. 
This command must be bind in `ivy-minibuffer-map'."
  (interactive)
  (ivy-set-action (lambda (selection)
										(ace-hacks--ivy-visit-action selection :up)))
  (ivy-done))

(defun ace-hacks-ivy-visit-split-down ()
  "Visit `ivy' selection on the down part of the split window
selected with `ace-window'. 
This command must be bind in `ivy-minibuffer-map'."
  (interactive)
  (ivy-set-action (lambda (selection)
										(ace-hacks--ivy-visit-action selection :down)))
  (ivy-done))

;;; Footer

(provide 'ace-hacks)
