(require 'outline)
(require 'js)

(setq js-indent-level 2)

(declare-function hydra-sp/sgml "ext:kb-sgml")

(defun ta-jsx-uncomment-line ()
  "Comment line of elements or components in js-jsx-mode."
  (back-to-indentation)
  (save-excursion
		(delete-char 3)
		(end-of-line)
		(delete-char -3)))

(defun ta-jsx-comment-line ()
  "Uncomment line of elements or components in js-jsx-mode."
  (back-to-indentation)
  (save-excursion
		(insert "{/*")
		(end-of-line)
		(insert "*/}")))

(defun ta-jsx-comment-or-uncomment-line ()
  "Comment element in js-jsx-mode."
  (interactive)
	(back-to-indentation)
  (if (looking-at "{/\\*")
      (ta-jsx-uncomment-line)
    (ta-jsx-comment-line)))

(defun ta-tide-mode-hook ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  ;; (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (eldoc-mode +1)
  ;; (tide-hl-identifier-mode +1)
	)

(defun ta-outline-js-mode-hook ()
  "Set up `outline-mode' and `bicycle'.  Specifically,

the variable `outline-regexp'."
  (outline-minor-mode t)
  (setq outline-regexp (concat
                        "//\\|"
                        "const\\|"
                        "[[:space:]]*app\\|"
                        "[[:space:]]*axios\\|"
                        "[[:space:]]*const")))

(add-hook 'js-mode-hook 'ta-outline-js-mode-hook)
(add-hook 'js-mode-hook #'ta-tide-mode-hook)

(define-key js-mode-map (kbd "<tab>") 'hydra-sgml/body)
(define-key js-mode-map (kbd "C-<tab>") 'hydra-sgml/body)


(provide 'setup-js-mode)
