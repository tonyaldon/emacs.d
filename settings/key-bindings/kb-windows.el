;;; Packages

(require 'ace-window)
(require 'framer)
(require 'kb)
(require 'transpose-frame)
(require 'windmove)

;;; Set up variables

(winner-mode t)

(setq windmove-wrap-around t)

(setq aw-char-position 'top-left)
(setq aw-ignore-current nil)
(setq aw-leading-char-style 'char)
(setq aw-background nil)
(setq aw-keys '(?s ?r ?d ?l ?t))
(setq aw-dispatch-always t)

(declare-function ta-term "ext:kb-term")

;;; Window commands

;;;; dired-mode

(defun ta--dired-side-by-side (current-directory)
  "Do the layout job of `ta-dired-side-by-side'."
  (delete-other-windows)
  (dired current-directory)
  (split-window-right))

(defun ta-dired-side-by-side ()
  "Pop two dired buffer side by side.

If `current-buffer' is visiting a file, the root directory of this
file becomes `dired-directory' of the popped dired buffers. If not
visiting a file, the $HOME directory is chosen to be the
`dired-directory'"
  (interactive)
  (let ((current-buffer-file-name (buffer-file-name))
        (current-dired-directory dired-directory))
    (cond
     (current-buffer-file-name
      (ta--dired-side-by-side (file-name-directory current-buffer-file-name)))
     (current-dired-directory
      (ta--dired-side-by-side current-dired-directory))
     (t
      (ta--dired-side-by-side (expand-file-name "~/"))))))

;;;; windmove enhancement

;;;;; advice

(defadvice windmove-do-window-select
    (before windmove-do-window-select-advice activate)
  "Push `selected-window' in the ring used by `aw-flip-window'.

The function `windmove-left', `windmove-right', `windmove-up' and
`windmove-down' are interactive wrappers to `windmove-do-window-select'."
  (aw--push-window (selected-window)))

;;;;; basic movements

;; negative argument (C--) is not much accesible in my keyboard layout,
;; but (C--) is really handy when used with the commands windmove-up,
;; windmove-down...
;; The simple solution to use the behaviour of windmove-up with
;; (C-u) instead of (C--) is to define the following commands,
;; so that: C-u M-x ta-windmove-up behave as C-- M-x windmove-up

(defun ta-windmove-left (&optional arg)
  "Select the window to the left of the current one.

This command exists just to have:
C-u M-x `ta-windmove-left' behave as C-- M-x `windmove-left'.

Note: negative argument (C--) is not much accesible in my keyboard layout,
but (C--) is really handy when used with the command `windmove-left'."
  (interactive "P")
  (windmove-do-window-select 'left (and arg -1)))

(defun ta-windmove-right (&optional arg)
  "Select the window to the right of the current one.

This command exists just to have:
C-u M-x `ta-windmove-right' behave as C-- M-x `windmove-right'.

Note: negative argument (C--) is not much accesible in my keyboard layout,
but (C--) is really handy when used with the command `windmove-right'."
  (interactive "P")
  (windmove-do-window-select 'right (and arg -1)))

(defun ta-windmove-up (&optional arg)
  "Select the window above the current one.

This command exists just to have:
C-u M-x `ta-windmove-up' behave as C-- M-x `windmove-up'.

Note: negative argument (C--) is not much accesible in my keyboard layout,
but (C--) is really handy when used with the command `windmove-up'."
  (interactive "P")
  (windmove-do-window-select 'up (and arg -1)))

(defun ta-windmove-down (&optional arg)
  "Select the window below the current one.

This command exists just to have:
C-u M-x `ta-windmove-down' behave as C-- M-x `windmove-down'.

Note: negative argument (C--) is not much accesible in my keyboard layout,
but (C--) is really handy when used with the command `windmove-down'."
  (interactive "P")
  (windmove-do-window-select 'down (and arg -1)))


(defun ta-swap-window ()
  "Swap buffers of current window and `next-window'."
  (interactive)
  (aw-swap-window (next-window)))

(defun ta-ansi-term-bash ()
  (interactive)
  (let ((term-name
         (s-concat "term:.../" (f-filename default-directory) "/")))
    (ansi-term "/bin/bash" term-name)))

(defun ta-ace-kill-buffer ()
  "Kill buffer in other window.

Other window is selected with `ace-window'."
  (interactive)
  (ace-select-window)
  (kill-this-buffer)
  (other-window))

(defhydra hydra-windows-size
  (:pre (set-cursor-color "#ffd500")
   :post (set-cursor-color "#26f9ad")
   :hint nil)
  ("b" (shrink-window-horizontally 5))
  ("f" (enlarge-window-horizontally 5))
  ("p" (shrink-window 5))
  ("n" (enlarge-window 5))
  ("q" nil))

(defun ta-clone-indirect-buffer (narrow)
  "Create an indirect buffer with name composed with NARROW string.

NARROW, a string, is the name of the section/function you are narrowing
in the indirect buffer.  The name of the indirect buffer is composed
with the `buffer-name' and NARROW.

The indirect buffer is displayed in the selected window.

See `clone-indirect-buffer'."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
         (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (read-string "Narrowed part name: "))))
  (let* ((newname (format "%s::%s" (buffer-name) narrow))
         (name (generate-new-buffer-name newname))
         (buffer (make-indirect-buffer (current-buffer) name t)))
    (switch-to-buffer buffer)
    buffer))

;;;; hydra-windows

(defhydra hydra-windows
  (:pre (progn
          (remove-hook 'post-command-hook 'insight-check-cursor-color)
          (set-cursor-color "#ffd500"))
   :post (progn
           (add-hook 'post-command-hook 'insight-check-cursor-color)
           (set-cursor-color "#26f9ad"))
   :hint nil)
  ("t" handy-line/body :color blue)
  ("b" ta-windmove-left)
  ("f" ta-windmove-right)
  ("p" ta-windmove-up)
  ("n" ta-windmove-down)
  ("M-t" transpose-frame)
  ("c" ace-window)
  ("'" aw-flip-window :color blue)
  ("<next>" window-toggle-side-windows)
  ("i" ta-clone-indirect-buffer)
  ("r" ta-ansi-term-bash :color blue)
  ("l" ta-dired-side-by-side)
  ("d" ace-delete-window)
  ("k" ta-ace-kill-buffer)
  ("u" winner-undo)
  ("]" winner-redo)
  ("." framer-push :color blue)
  ("x" framer-undo)
  (":" framer-redo)
  (">" make-frame :color blue)
  ("s" hydra-windows-size/body :color blue)
  ("+" balance-windows)
  ("M-+" balance-windows-area)
  ("q" nil))


(global-set-key (kbd "C-o") 'delete-other-windows)
(global-set-key (kbd "M-o") 'delete-window)


(global-set-key (kbd "<f7>") 'winner-undo)
(global-set-key (kbd "C-+") 'winner-redo)

(global-set-key (kbd "M-u") 'hydra-windows/body)

(global-set-key (kbd "M-b") 'ta-windmove-left)
(global-set-key (kbd "M-f") 'ta-windmove-right)
(global-set-key (kbd "M-p") 'ta-windmove-up)
(global-set-key (kbd "M-n") 'ta-windmove-down)
(define-key Info-mode-map (kbd "M-n") 'windmove-down)

(define-key dired-mode-map (kbd "C-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)
(define-key term-raw-map (kbd "M-o") 'delete-window)
(define-key term-mode-map (kbd "M-o") 'delete-window)


;;; Footer

(provide 'kb-windows)
