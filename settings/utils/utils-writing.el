(defun ta-unfill-paragraph ()
  "Transform a paragraph into a single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil t)))

(defun ta-mark-next-phrase ()
  "Mark the phrase after `point'."
  (interactive)
  (unless (re-search-backward "\\([.?!]\\)\\|\\(^$\\)" nil t)
    (error "Can't mark this phrase. You are not inside a phrase."))
  (forward-char 1)
  (push-mark)
  (activate-mark)
  (unless (re-search-forward "[.!?]" nil t)
    (deactivate-mark)
    (error "Can't mark this phrase. You are not inside a phrase.")))

(global-set-key (kbd "C-<f1>") 'ta-mark-next-phrase)

(provide 'util-writing)
