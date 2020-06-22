(require 'kb)
(require 'setup-sgml-mode)
(require 'setup-emmet-mode)
(require 'css-mode)

(declare-function hydra-sp/body "ext:kb-smartparens")

(defhydra hydra-sgml
  (
   :pre (hydra-color-pre-sgml)
   :post (hydra-color-post)
   :hint nil)
  ("t" hydra-sp/body :color blue)
	("DEL" sgml-delete-tag)
  ("p" ta-previous-attribute)
  ("n" ta-next-attribute)
  ("b" emmet-prev-edit-point)
  ("f" emmet-next-edit-point)
  ("&" sgml-namify-char)
  ("." emmet-wrap-with-markup)
  ("c" sgml-close-tag)
	("e" emmet-expand-yas)
  ("i" emmet-expand-line)
  ;; ---
  ("g" cleanup-buffer)
  ("M--" undo)
  ("q" nil))

(define-key sgml-mode-map (kbd "<tab>") 'hydra-sgml/body)
(define-key sgml-mode-map (kbd "C-<tab>") 'hydra-sgml/body)
(define-key css-mode-map (kbd "<tab>") 'hydra-sgml/body)
(define-key css-mode-map (kbd "C-<tab>") 'hydra-sgml/body)
(define-key sgml-mode-map (kbd "C->") (lambda () (interactive) (insert ">")))
(define-key css-mode-map (kbd "C->") (lambda () (interactive) (insert ">")))


(defadvice sgml-delete-tag (after sgml-delete-tag-advice activate)
  (indent-buffer))

(provide 'kb-sgml)
