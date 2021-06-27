(require 's)

(defun istring-delete-region (region-beg region-end)
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

(defun istring-lower-camel-case (region-beg region-end)
  "Make `s-lower-camel-case' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-lower-camel-case
           (istring-delete-region region-beg region-end))))

(defun istring-upper-camel-case (region-beg region-end)
  "Make `s-upper-camel-case' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-upper-camel-case
           (istring-delete-region region-beg region-end))))

(defun istring-snake-case (region-beg region-end)
  "Make `s-snake-case' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-snake-case
           (istring-delete-region region-beg region-end))))

(defun istring-dashed-words (region-beg region-end)
  "Make `s-dashed-words' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-dashed-words
           (istring-delete-region region-beg region-end))))


(defun istring-upcase (region-beg region-end)
  "Make `s-upcase' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-upcase
           (istring-delete-region region-beg region-end))))

(defun istring-downcase (region-beg region-end)
  "Make `s-downcase' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-downcase
           (istring-delete-region region-beg region-end))))

(defun istring-capitalize (region-beg region-end)
  "Make `s-capitalize' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-capitalize
           (istring-delete-region region-beg region-end))))

(defun istring-titleize (region-beg region-end)
  "Make `s-titleize' interactive. Modify sexp at point or the
region if actived."
  (interactive "r")
  (insert (s-titleize
           (istring-delete-region region-beg region-end))))

(defun istring-collapse-whitespace ()
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

(defun istring-wrap (region-beg region-end)
  "Wrap region with first and second sentences separeted by a space given in the prompt."
  (interactive "r")
  (setq wrappers (s-split " " (read-string "Wrap region with: ")))
  (let ((wrap-beg (s-concat (car wrappers) "\n"))
        (wrap-end (s-concat "\n" (car (cdr wrappers)))))
    (message "%s" wrap-end)
    (insert (s-wrap
             (istring-delete-region region-beg region-end)
             wrap-beg
             wrap-end))))

(provide 'interactive-string)
