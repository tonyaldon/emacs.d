(require 'kb)

(defhydra hydra-narrow (:pre (hydra-color-pre)
                             :post (hydra-color-post)
                             :hint nil)
  ;; "narrow"
  ("w" widen :color blue)
  ("n" narrow-to-region :color blue)
  ("f" narrow-to-defun :color blue))

(key-chord-define-global "tq" 'hydra-narrow/body)

(defadvice narrow-to-region (after ta-narrow-deactive-mark-advice activate)
  (if mark-active (set-mark-command t)))


(provide 'kb-narrow)
