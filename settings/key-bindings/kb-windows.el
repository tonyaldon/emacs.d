;;; Packages

(require 'framer)
(require 'kb)
(require 'transpose-frame)

;;; Set up variables

(winner-mode t)

(defun ta-swap-window ()
  "Swap buffers of current window and `next-window'."
  (interactive)
  (let ((buffer1 (current-buffer))
        (buffer2 (window-buffer (next-window)))
        (win (next-window)))
    (set-window-buffer (selected-window) buffer2)
    (set-window-buffer (next-window) buffer1)
    (select-window win)))

(defun ta-ansi-term-bash ()
  (interactive)
  (let ((term-name
         (s-concat "term:.../" (f-filename default-directory) "/")))
    (ansi-term "/bin/bash" term-name)))

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
  ("M-t" transpose-frame)
  ("i" ta-clone-indirect-buffer)
  ("r" ta-ansi-term-bash :color blue)
  ("u" winner-undo)
  ("]" winner-redo)
  ("." framer-push :color blue)
  ("x" framer-undo)
  (":" framer-redo)
  ("q" nil))


(global-set-key (kbd "C-o") 'delete-other-windows)
(global-set-key (kbd "M-o") 'delete-window)


(global-set-key (kbd "<f7>") 'winner-undo)
(global-set-key (kbd "C-+") 'winner-redo)

(global-set-key (kbd "M-u") 'hydra-windows/body)


(define-key dired-mode-map (kbd "C-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)
(define-key term-raw-map (kbd "M-o") 'delete-window)
(define-key term-mode-map (kbd "M-o") 'delete-window)


;;; Footer

(provide 'kb-windows)
