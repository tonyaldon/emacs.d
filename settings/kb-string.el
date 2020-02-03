(require 's)

(defun ta-ss-delete-region (region-beg region-end)
  "Delete active region or `sexp-at-point', set `point' as
`delete-region' does and return the deleted string."
  (interactive "r")
  (if (use-region-p)
      (progn
        (setq s-beg region-beg)
        (setq s-end region-end))
    (setq s-beg (beginning-of-thing 'sexp))
    (setq s-end (end-of-thing 'sexp)))
  (setq s-in-region (buffer-substring s-beg s-end))
  (delete-region s-beg s-end)
  s-in-region)

(defun ta-ss-s-lower-camel-case (region-beg region-end)
  "Make `s-lower-camel-case' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-lower-camel-case
           (ta-ss-delete-region region-beg region-end))))

(defun ta-ss-s-upper-camel-case (region-beg region-end)
  "Make `s-upper-camel-case' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-upper-camel-case
           (ta-ss-delete-region region-beg region-end))))

(defun ta-ss-s-snake-case (region-beg region-end)
  "Make `s-snake-case' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-snake-case
           (ta-ss-delete-region region-beg region-end))))

(defun ta-ss-s-dashed-words (region-beg region-end)
  "Make `s-dashed-words' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-dashed-words
           (ta-ss-delete-region region-beg region-end))))


(defun ta-ss-s-upcase (region-beg region-end)
  "Make `s-upcase' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-upcase
           (ta-ss-delete-region region-beg region-end))))

(defun ta-ss-s-downcase (region-beg region-end)
  "Make `s-downcase' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-downcase
           (ta-ss-delete-region region-beg region-end))))

(defun ta-ss-s-capitalize (region-beg region-end)
  "Make `s-capitalize' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-capitalize
           (ta-ss-delete-region region-beg region-end))))

(defun ta-ss-s-titleize (region-beg region-end)
  "Make `s-titleize' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-titleize
           (ta-ss-delete-region region-beg region-end))))

(defun ta-ss-s-collapse-whitespace ()
  "Apply `s-collapse-whitespace' on the current line."
  (interactive)
  (let ((s-beg
         (progn
           (back-to-indentation)
           (point)))
        (s-end
         (progn
           (end-of-line)
           (point))))
    (setq s-string (buffer-substring s-beg s-end))
    (delete-region s-beg s-end)
    (goto-char s-beg)
    (insert (s-collapse-whitespace s-string))
    (goto-char s-beg)))

(defun ta-ss-wrap (region-beg region-end)
  "Wrap region with first and second sentences separeted by a space given in the prompt."
  (interactive "r")
  (setq wrappers (s-split " " (read-string "Wrap region with: ")))
	(let ((wrap-beg (s-concat (car wrappers) "\n"))
				(wrap-end (s-concat "\n" (car (cdr wrappers)))))
		(message "%s" wrap-end)
		(insert (s-wrap
						 (ta-ss-delete-region region-beg region-end)
						 wrap-beg
						 wrap-end))))

(defhydra hydra-ss
  (
   :pre (hydra-color-pre)
   :post (hydra-color-post)
   :hint nil)
  ("d" ta-ss-s-dashed-words :color blue)
  ("s" ta-ss-s-snake-case :color blue)
  ("l" ta-ss-s-lower-camel-case :color blue)
  ("r" ta-ss-s-upper-camel-case :color blue)
  ;; ---
  ("<up>" ta-ss-s-upcase :color blue)
  ("<down>" ta-ss-s-downcase :color blue)
  ("t" ta-ss-s-titleize :color blue)
  ("c" ta-ss-s-capitalize :color blue)
  ;; ---
	("," ta-ss-s-collapse-whitespace :color blue)
	("e" ta-ss-wrap :color blue)
  ;; ---
  ("M--" undo)
  ("q" nil))

(global-set-key (kbd "M-<prior>") 'hydra-ss/body)

(provide 'kb-string)
