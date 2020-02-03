(require 'kb)
(require 'kb-smartparens)
(require 'kb-lines)

(defhydra hydra-kmacro
  (
   :pre (hydra-color-pre)
   :post (hydra-color-post)
   :hint nil)
  ("<next>" hydra-lines/body :color blue)
  ("<prior>" hydra-sp-reshape/body :color blue)
  ("." hydra-sp/body :color blue)
  ;; ---
  ("s" kmacro-set-counter :color blue)
  ("r" kmacro-insert-counter)
  ;; ---
  ("l" kmacro-end-or-call-macro)
  ;; ---
  ("<up>" kmacro-cycle-ring-previous :color blue)
  ("<down>" kmacro-cycle-ring-next :color blue)
  ("n" kmacro-name-last-macro :color blue)
  ("b" kmacro-bind-to-key :color blue)
  ("e" kmacro-edit-macro :color blue)
	;; ---
  ("M--" undo)
  ("q" nil))


(global-set-key (kbd "M-<next>") 'hydra-kmacro/body)
(global-set-key (kbd "M-(") 'kmacro-start-macro)
(global-set-key (kbd "M-)") 'kmacro-end-macro)


(provide 'kb-kmacro)
