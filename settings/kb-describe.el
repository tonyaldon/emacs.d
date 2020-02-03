(require 'kb)
(require 'yasnippet)

(defun ta-describe-thing-at-point ()
  "Display the full documentation of the `thing-at-point'

that is either a function or a variable.
Return nil if the symbol of the `thing-at-point' is neither a function
nor a variable."
  (interactive)
  (let ((current-symbol (symbol-at-point)))
    (cond
     ((not current-symbol))
     ((boundp current-symbol) (describe-variable current-symbol))
     ((fboundp current-symbol) (describe-function current-symbol))
     (t (message "The symbol-at-point is neither a variable or a function")))))

(defhydra hydra-describe
  (
   :pre (hydra-color-pre)
   :post (hydra-color-post)
   :hint nil)
  ("o" describe-key :color blue)
  ("d" describe-function :color blue)
  ("l" describe-variable :color blue)
  ;; ---
  ("s" counsel-imenu :color blue)
  ("r" yas-describe-tables :color blue)
  ("m" describe-mode :color blue)
  ;; ---
  ("e" apropos :color blue)
  ("i" info :color blue)
  ("f" list-faces-display :color blue)
  ("c" customize :color blue)
  ;; ---
  ("M--" undo)
  ("q" nil))

(key-chord-define-global "xo" 'hydra-describe/body)
(key-chord-define-global "bn" 'ta-describe-thing-at-point)



(provide 'kb-describe)
