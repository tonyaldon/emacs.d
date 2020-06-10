(require 'kb)
(require 'kb-lines)
(require 'setup-sgml-mode)
(require 'emmet-mode)

(defhydra hydra-sgml
  (
   :pre (hydra-color-pre-sgml)
   :post (hydra-color-post)
   :hint nil)
	("DEL" sgml-delete-tag)
  ("p" ta-previous-attribute)
  ("n" ta-next-attribute)
  ("b" emmet-prev-edit-point)
  ("f" emmet-next-edit-point)
	("a" sgml-namify-char)
	("u" emmet-wrap-with-markup)
	("i" sgml-close-tag)
	("e" emmet-expand-yas)
	("TAB" emmet-expand-line)
  ;; ---
  ("g" cleanup-buffer)
  ("M--" undo)
	("q" nil))

(defadvice sgml-delete-tag (after sgml-delete-tag-advice activate)
	(indent-buffer))

(provide 'kb-sgml)
