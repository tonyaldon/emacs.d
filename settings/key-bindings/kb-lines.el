(require 'kb)
(require 'drag-stuff)
(load "~/.emacs.d/.cask/28.0/elpa/drag-stuff-20161108.749/drag-stuff.el")

(defvar hydra-lines-active nil)

(defun hydra-lines-active ()
  "Toggle status of `hydra-lines-active'"
  (if hydra-lines-active
      (setq hydra-lines-active nil)
    (setq hydra-lines-active t)))

(defhydra hydra-lines
  (:pre (progn (if insight-mode (insight-mode -1))
               (set-cursor-color "#fa87ce"))
   :post (progn (set-cursor-color "#26f9ad")
                (hydra-lines-active))
   :hint nil)
  ("M-l" recenter-top-bottom)
  ("t" handy-sexp/body :color blue)
  (";" handy-line-comment)
  ("DEL" delete-backward-char)
  ("~" set-mark-command)
  ("m" exchange-point-and-mark)
  ;; action on line(s)
  ("!" flush-lines)
  ("?" keep-lines)
  ;; current line
  ("k" kill-line)
  ("l" (kill-line 0))
  ("x" handy-line-kill)
  ("y" handy-line-copy-paste-below)
  ("r" join-line)
  ("o" open-line)
  ("O" delete-blank-lines)
  ("," handy-cycle-spacing)
  ;; to insert text
  ("u" handy-line-add-above :color blue)
  ("]" handy-line-add-below :color blue)
  ("_" handy-add-space :color blue)
  ;; quick motions
  ("n" next-logical-line)
  ("p" previous-logical-line)
  ("f" forward-char)
  ("b" backward-char)
  ("i" back-to-indentation)
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("M-f" forward-word)
  ("M-b" backward-word)
  ("M-e" org-forward-sentence)
  ("M-a" org-backward-sentence)
  ("C-M-a" iy-go-to-char-backward :color blue)
  ("C-M-e" iy-go-to-char :color blue)
  ;; drag stuff
  ("d" drag-stuff-up)
  ("s" drag-stuff-down)
  ("<left>" drag-stuff-left)
  ("<right>" drag-stuff-right)
  ;; clean/undo/nil
  ("M--" undo)
  ("q" nil))

(defadvice move-beginning-of-line (before move-beginning-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice move-end-of-line (before move-end-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice hydra-lines/body (before hydra-lines-advice activate)
  (hydra-lines-active))

(key-chord-define-global "ld" 'hydra-lines/body)


(provide 'kb-lines)
