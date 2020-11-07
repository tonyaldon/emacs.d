(require 'dired)
(require 'dired-hacks-utils)
(require 'ibuffer)
(require 'ibuf-ext)

;;; Utility functions

;;;; Windows

(defun sidebar--dired-width (dir)
  "Return the number of characters of the bigger file or directory in

a dired buffer generate with DIR as `dired-directory'."
  (with-current-buffer (dired-noselect dir)
    (-max (--map (length (-last-item (s-split "/" it)))
                 (dired-utils-get-all-files)))))

(defun sidebar--delete-window (window mode)
  "Delete WINDOW if in MODE mode."
  (select-window window)
  (when (equal major-mode mode)
		(delete-window)))

(defun sidebar--delete-dired-windows ()
  "Delete `dired-mode' windows in `window-list'."
	(save-selected-window
		(--each (window-list) (sidebar--delete-window it 'dired-mode))))

(defun sidebar--delete-ibuffer-windows ()
  "Delete `ibuffer-mode' windows in `window-list'."
  (save-selected-window
		(--each (window-list) (sidebar--delete-window it 'ibuffer-mode))))

(defun sidebar-delete-windows ()
  "Delete `ibuffer-mode' and `dired-mode' windows in `window-list'."
  (sidebar--delete-dired-windows)
  (sidebar--delete-ibuffer-windows))

;;;; Side windows

(defun sidebar--side-window-to-window ()
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

(defun sidebar--side-window-p (window)
  "Return t if WINDOW is a side window."
  (-contains?
   (-map 'car (window-parameters window))
   'window-side))

(defun sidebar-delete-other-windows ()
  "Delete other windows also when the `selected-window' is a side window."
  (interactive)
  (let ((side-window-p (sidebar--side-window-p (selected-window))))
    (if side-window-p (sidebar--side-window-to-window) (delete-other-windows))))

;;;; Frame

(defun sidebar--window-top-left ()
  "Return the window at the top left corner of the current frame."
  (car (--filter (equal '(0 0) (-take 2 (window-edges it)))
								 (window-list))))

(defun sidebar--window-mode (window)
  "Return the `major-mode' of WINDOW."
	(save-selected-window
    (select-window window)
    major-mode))

(defun sidebar-frame-p ()
  "Return true if there is a sidebar display in the frame."
  (interactive)
	(save-selected-window
    (let ((window-top-left (sidebar--window-top-left)))
			(and (equal (sidebar--window-mode window-top-left) 'dired-mode)
					 (progn (select-window window-top-left)
									(ignore-errors (windmove-down))
									(equal (sidebar--window-mode (selected-window))
												 'ibuffer-mode))))))

;;;; Ibuffer

(defun sidebar-project-name (dir)
  "Return the name of the project the DIR belongs too.

If DIR doesn't belongs to any project, DIR plays the 'role' of project.

See `project-current'."
  (let ((proj dir))
    (when-let (proj-cur (project-current nil dir))
      (setq proj (cdr proj-cur)))
    (nth 1 (nreverse (s-split "/" proj)))))

(defun sidebar-ibuffer-buffer-list (dir)
  "Return buffer list of the project the DIR belongs too.

If DIR doesn't belongs to any project, DIR plays the 'role' of project.
In this case, only buffers with `buffer-file-name' are kept.

See `project-current'."
  (if-let (proj (project-current nil dir))
      (project--buffer-list proj)
    (--filter
     (when-let (bfname (buffer-file-name it))
       (s-contains-p dir bfname))
     (buffer-list))))

(define-ibuffer-filter sidebar-filter
    "Show Ibuffer with all buffers in the `sidebar-ibuffer-buffer-list'."
  (:description nil)
  (with-current-buffer buf
    (-contains-p qualifier buf)))

;;; Sidebar

(setq sidebar-window-state nil)

(defun sidebar ()
  "Pop up a dired buffer on the left of the frame where `dired-directory'

is the `default-directory' of the current buffer.
If the frame contains buffers in `dired-mode', delete them."
  (interactive)
  (cond ((and (sidebar-frame-p)
							(not (eq last-command 'sidebar)))
				 (sidebar-delete-windows))
				((and (eq last-command 'sidebar) sidebar-window-state)
				 (progn
					 (window-state-put sidebar-window-state)
					 (setq sidebar-window-state nil)))
				(t
				 (setq sidebar-window-state (window-state-get))
				 (sidebar-delete-windows)
				 (sidebar-delete-other-windows)
				 (let* ((dir default-directory)
								(ibuffer-buffer-name (format "*sidebar-%s*"
																						 (sidebar-project-name dir))))
					 (split-window-right (+ (sidebar--dired-width dir) 10)) ; 10 is arbitrary margin
					 (ibuffer nil ibuffer-buffer-name
										(list (cons 'sidebar-filter (sidebar-ibuffer-buffer-list dir))))
					 ;; TODO: ibuffer-formats ((modified vc-status-mini " " name))
					 (setq-local ibuffer-formats (-concat '((modified " " name)) ibuffer-formats))
					 ;; TODO: set ibuffer-groups to (ta-ibuffer-vc-generate-filter-groups-by-vc-root)
					 (split-window-below)
					 (dired dir)))))

;;; Footer

(provide 'sidebar)
