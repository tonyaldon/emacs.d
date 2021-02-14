;;; Packages

(require 'kb)
(require 'multiple-cursors)
(require 'expand-region)
(require 'iedit)
(require 'smartparens)

;;; Global variables

(setq mark-ring-max 8)
(setq global-mark-ring-max 8)
(setq expand-region-preferred-python-mode 'fgallina-python)

;;; iedit-mode

(defun ta-mouse-iedit-mode ()
  "Toggle `iedit-mode' on mouse click."
  (interactive)
  (call-interactively 'mouse-set-point)
  (call-interactively 'iedit-mode))

(defun ta-advice-mouse-set-point-iedit-mode (&rest r)
  "Turn off `iedit-mode' if already on.

Intended to be use as advice before of `mouse-set-point'."
  (when iedit-mode (call-interactively 'iedit-mode)))

(advice-add 'mouse-set-point :before 'ta-advice-mouse-set-point-iedit-mode)

;;; hydra-mc

(defhydra hydra-mc
  (:pre (progn
          (if insight-mode (insight-mode -1))
          (set-cursor-color "#87cefa"))
   :post (set-cursor-color "#26f9ad")
   :hint nil)
  ("t" handy-line/body :color blue)
  ("p" mc/mark-previous-like-this)
  ("n" mc/mark-next-like-this)
  ("b" mc/mark-previous-like-this-word)
  ("f" mc/mark-next-like-this-word)
  ("s" mc/unmark-previous-like-this)
  ("d" mc/unmark-next-like-this)
  ("/" mc/mark-sgml-tag-pair)
  ("i" mc/insert-numbers)
  ;; TODO: see all cool commands of mc/... all, dwim, defun
  ("a" mc/mark-all-in-region)
  ;; FIXME: is it a good place here to have 'replace' commands
  ("M-c" query-replace :color blue)
  ("c" query-replace-regexp :color blue)
  ("M-s" replace-string :color blue)
  ("M-r" replace-regexp :color blue)
  ("q" nil))

(defadvice mc/keyboard-quit (after ta-mc/keyboard-quit-advice activate)
  (set-cursor-color "#26f9ad"))

;;; Key bindings

(global-set-key (kbd "C-p") 'handy-mark-line)
(global-set-key (kbd "C-b") 'handy-expand-region-dwim)
(global-set-key (kbd "C-n") 'handy-mark-dwim)
(global-set-key (kbd "C-f") 'handy-mark-inside-dwim)

(global-set-key (kbd "C-l") 'handy-mark-pop-local)
(global-set-key (kbd "M-<return>") 'newline)
(global-set-key (kbd "M-c") 'hydra-mc/body)
(global-set-key (kbd "<C-down-mouse-1>") 'mc/add-cursor-on-click)

(key-chord-define-global "{p" 'iedit-mode)
(global-set-key (kbd "<mouse-3>") 'ta-mouse-iedit-mode)

;;; TODO
;; TODO: find a way to use these function
;; (iedit-mode-toggle-on-function)
;; (iedit-restrict-current-line)
;; (iedit-restrict-function)
;; (iedit-restrict-region)

;; TODO: multiple cursors
;; TODO: iedit
;; TODO: replace

;;; Footer

(provide 'kb-mark)
