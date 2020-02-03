(require 'kb)
(require 'org)
(require 'org-bullets)

(setq org-edit-src-content-indentation 0)
(setq org-return-follows-link t)
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)))
(defun ta-org-confirm-babel-evaluate (lang body)
  (not (string= lang "emacs-lisp")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'ta-org-confirm-babel-evaluate)
(setq system-time-locale "C")


(defhydra hydra-org
  (
   :pre (hydra-color-pre-org)
   :post (hydra-color-post)
   :hint nil)
  ("<prior>" hydra-sp-reshape/body :color blue)
  ("<next>" hydra-lines/body :color blue)
  ("." hydra-sp/body :color blue)
  ("j" hydra-org/body :color blue)
  ("m" hydra-scrolling/body :color blue)
  ;; ---
  ("M-l" org-mark-ring-goto)
	("c" org-copy-subtree)
  ("k" org-kill-line)
  ("p" outline-previous-visible-heading)
  ("n" outline-next-visible-heading)
  ("b" org-backward-heading-same-level)
  ("f" org-forward-heading-same-level)
  ("Y" outline-up-heading)
  ("u" org-down-element)
  ("y" org-up-element)
  ;; ("c RET" org-insert-heading-after-current)
  ("RET" org-meta-return :color blue)
  ("i" org-ctrl-c-minus)
  ("*" org-ctrl-c-star)
  (">" org-metaright)
  ("<" org-metaleft)
  (")" org-shiftmetaright)
  ("(" org-shiftmetaleft)
  ("<up>" org-shiftmetaup)
  ("<down>" org-shiftmetadown)
  ("d" org-shiftup)
  ("s" org-shiftdown)
  ("^" org-sort)
  ("[" org-narrow-to-subtree)
  ("]" widen)
  ("<tab>" org-cycle)
	("o" org-open-at-point)
  ("q" nil))


(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'org-bullets-mode)

(key-chord-define-global "pd" 'hydra-org/body)

(provide 'kb-org)
