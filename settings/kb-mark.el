(require 'kb)
(require 'multiple-cursors)
(require 'expand-region)
(require 'iedit)
(require 'smartparens)

(setq mark-ring-max 4)
(setq global-mark-ring-max 4)

(defun ta-pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun ta--point-at-beginnig-sp-sexp-p ()
  "Return non-nil if `point' is at the beginning of a sp-sexp

and :op non empty. See `sp-get-thing'."
  (let ((ok (sp-get-thing)))
    (when ok
      (when (and (eq (point) (sp-get ok :beg))
                 (not (string-empty-p (sp-get ok :op))))
        (point)))))

(defun ta--mark-sexp-at-point ()
  "Mark the `sexp' at point."
  (let ((sexp-beg (beginning-of-thing 'sexp))
        (sexp-end (end-of-thing 'sexp)))
    (goto-char sexp-end)
    (set-mark sexp-end)
    (goto-char sexp-beg)))

(defun ta-mark-sexp-at-point ()
  "Mark the `sexp' at point. See `sexp-at-point' and `sp-mark-sexp'."
  (interactive)
  (if (or (ta--point-at-beginnig-sp-sexp-p)
          (eq (following-char) ?<))
      (sp-mark-sexp)
    (if (eq (preceding-char) ?\")
        (progn
          (sp-backward-sexp)
          (sp-mark-sexp))
      (if (and (memq (following-char) '(32 ?\) ?\] ?\} ?>))
               (looking-back "[[:alnum:]]" 1))
          (backward-char 1))
      (ta--mark-sexp-at-point))))

;; TODO
;; ta-mark-attribute -> thing like this: text="blabla"
;; - if inside the string -> goto to beginning (outside the first double quote)
;; - goto backward to [[:space]]
;; - ?? see the case where cursor is after the second double quote "
;; this regexp ----> "[[:space:]]\\{1\\}[a-z]*=[\"][[:alnum:].&:?,= /-]*[\"]"
;; matches all the attributes in the piece of html below
;; <meta charset="UTF-8"/>
;; <meta name="viewport"
;;       content="width=device-width, initial-scale=1.0"/>
;; <link href="css/style.css" rel="stylesheet"/>
;; <link href="https://fonts.googleapis.com/css?family=Quicksand&display=swap" rel="stylesheet">



(defun ta-point-in-string-p (pt)
  "Returns t if PT is in a string"
  (eq 'string (syntax-ppss-context (syntax-ppss pt))))

(defun ta-goto-begining-of-string (pt)
  "Go to begining of the string if PT is inside a string.
Return nil if PT isn't inside a string. See the function `ta-point-in-string-p'"
  (if (ta-point-in-string-p pt)
      (goto-char (nth 8 (syntax-ppss pt)))
    nil))

(defun ta-mark-inside-pairs ()
  "An other way to do `er/mark-inside-pairs' but work for sgml-tag too."
  (interactive)
  (ta-goto-begining-of-string (point))
  ;; todo: do thing when inside a tag <tag name="tony"> (maybe use the function sgml-begining-of-tag)
  (sp-backward-up-sexp)
  (sp-mark-sexp)
  (sp-down-sexp)
  (exchange-point-and-mark)
  (sp-backward-down-sexp)
  (exchange-point-and-mark))

(defun ta-toggle-mark ()
  "Bind M-[ to `er/mark-word' when emacs not use in terminal"
  (interactive)
	(unless (lookup-key global-map (kbd "M-["))
		(global-set-key (kbd "M-[") 'er/mark-word)))

(defhydra hydra-mc (
                    :pre (hydra-color-pre-mc)
                    :post (hydra-color-post)
                    :hint nil)
  ("p" mc/mark-previous-like-this)
  ("n" mc/mark-next-like-this)
  ("b" mc/mark-previous-like-this-word)
  ("f" mc/mark-next-like-this-word)
  ("s" mc/unmark-previous-like-this)
  ("d" mc/unmark-next-like-this)
  ("t" mc/mark-sgml-tag-pair)
  ("q" nil))

(defadvice mc/keyboard-quit (after ta-mc/keyboard-quit-advice activate)
  (hydra-color-post))

(global-set-key (kbd "M-l") 'ta-pop-local-mark-ring)
(global-set-key (kbd "M-<return>") 'newline)
(global-set-key (kbd "M-c") 'hydra-mc/body)
(global-set-key (kbd "<mouse-3>") 'mc/add-cursor-on-click)

(key-chord-define-global ">p" 'iedit-mode)

(global-set-key (kbd "<prior>") 'ta-mark-inside-pairs)
(global-set-key (kbd "<next>") 'er/mark-inside-quotes)
(global-set-key (kbd "<up>") 'ta-mark-sexp-at-point)
;; (global-set-key (kbd "M-]") 'er/expand-region)
(global-set-key (kbd "M-[") 'er/mark-word)
(global-set-key (kbd "M-à") 'er/mark-word)

(global-set-key (kbd "<f7>") 'ta-toggle-mark)

(provide 'kb-mark)
