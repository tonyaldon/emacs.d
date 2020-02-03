(require 'latex)
(require 'tex-site)





(add-to-list 'LaTeX-verbatim-environments "minted")
;; we can manualy add other mintinline{shell} to support more languages
;; it is better to do this as my latex document grow
(add-to-list 'LaTeX-verbatim-macros-with-delims "mintinline{mysql}")

;; font
;; see font-latex-verbatim-face



(provide 'setup-latex-mode)
