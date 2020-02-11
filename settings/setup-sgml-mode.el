(require 'sgml-mode)

(defconst ta-sgml-attr-value-re "\\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)")
(defconst ta-sgml-name-re sgml-name-re)
(defconst ta-sgml-attr-re
  (concat "[[:space:]]\\{1\\}" ta-sgml-name-re "=" ta-sgml-attr-value-re))

(defun ta-next-attribute ()
  "Move forward to the next attribute"
  (interactive)
  (save-excursion (setq beg-of-tag (sgml-beginning-of-tag)))
  (if beg-of-tag
      (progn
        (looking-at "\\(/>\\|>\\)")
        (let ((end-of-tag-symbol (match-string 0)))
          (cond
           ((equal end-of-tag-symbol "/>") (forward-char 2))
           ((equal end-of-tag-symbol ">") (forward-char 1))
           (t (let ((end-of-tag
                     (save-excursion
                       (sgml-tag-end (car (sgml-get-context))))))
                (if (search-forward-regexp ta-sgml-attr-re end-of-tag t)
                    (progn (goto-char (match-beginning 0))
                           (forward-char 1))
                  (goto-char end-of-tag)
                  (if (looking-back "/>")
                      (backward-char 2)
                    (backward-char 1))))))))
    ;;2nd if part
    (search-forward-regexp sgml-tag-name-re nil t)
    (let ((tag-type (substring (match-string 0) 0 2)))
      (if (equal tag-type "</")
          (sgml-skip-tag-forward 1)
        (unless (looking-at ">")
          (progn
            (search-forward-regexp ta-sgml-attr-re nil t)
            (goto-char (match-beginning 0))
            (forward-char 1)))))))

(defun ta-previous-attribute ()
  "Move forward to the next attribute"
  (interactive)
  (save-excursion
    (setq inside-tag (sgml-beginning-of-tag))
    (if inside-tag (setq beg-of-tag (point))))
  (if inside-tag
      (if (search-backward-regexp ta-sgml-attr-re beg-of-tag t)
          (forward-char 1)
        (goto-char beg-of-tag))
    (search-backward-regexp ">" nil t)
		(save-excursion (setq tag-type
													(sgml-tag-type (car (sgml-get-context)))))
		(cond
		 ((equal tag-type 'empty) (backward-char 1))
		 ((equal tag-type 'close) (sgml-beginning-of-tag)))))



(provide 'setup-sgml-mode)
