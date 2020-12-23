;;; insight.el --- Minor mode to browse buffers -*- lexical-binding: t -*-

;; Copyright (C) 2020  Tony aldon

;; Author: Tony Aldon
;; Maintainer: Tony Aldon <tony.aldon.adm@gmail.com>
;; URL: ???
;; Version: 0.1.0
;; Keywords: scroll, zoom, narrow, outline
;; Package-Requires: ???

;;; Commentary:
;;
;; Insight enhances your browsing experience and the insight you have of
;; your displayed buffers. `insight-mode' mode binds one character keys
;; to various scrolling commands, zooming commands, narrowing commands and
;; `outline-minor-mode' navigating commands.

;; Bind globaly `insight-mode' to any keys that is convenient for your
;; usage, e.g:

;; (global-set-key (kbd "M-i") 'insight-mode)

;; Enable it anytime you want to scroll, zoom, narrow or navigate your code.
;; You can disable it by pressing the 'q' key .

;; If the one character keys don't fit your workflow, change the bindings in
;; `insight-mode-map'.

;; With `insight-ace-window' command, you can interactively choose the displayed
;; buffer to be scroll by commands like `scroll-other-window' and
;; `scroll-other-window-down'. To work well we advice `scroll-other-window'
;; and `scroll-other-window-down' commmands. To do so, add this line to your
;; emacs configuration file:

;; (insight-set-window-advices)

;; At any moment you can revoke it by running: M-x insight-unset-advices.

;; To set up `insight-mode' to change the cursor color to `insight-cursor-color'
;; when `insight-mode' is turned on, add this line to your emacs configuration file:

;; (insight-use-cursor-color)

;; You can set the cursor color in `insight-mode', for instance to blue (#0000ff),
;; by setting the variable `insight-cursor-color' like this:

;; (setq insight-cursor-color "#0000ff")

;; So a standard configuration with `insight-cursor-color' blue is:

;; (setq insight-cursor-color "#0000ff")
;; (insight-use-cursor-color)
;; (insight-set-window-advices)

;;; Change Log: ???

;;; Code:

;;; Cursor

(defvar insight-cursor-color "#fd971f"
  "Color of the cursor when `insight-mode' is turned on.")

(setq insight-use-cursor-color nil)
(setq insight-cursor-color-default nil)

(defun insight-check-cursor-color ()
  "Check if the cursor color is well set according to `insight-mode'."
  (interactive)
  (let ((cursor-color (face-attribute 'cursor :background)))
    (if insight-mode
        (unless (string= cursor-color insight-cursor-color)
          (set-cursor-color insight-cursor-color))
      (when (string= cursor-color insight-cursor-color)
        (set-cursor-color insight-cursor-color-default)))))

(defun insight-use-cursor-color ()
  "Set up `insight-mode' to change the cursor color to `insight-cursor-color'
when `insight-mode' is turned on."
  (interactive)
  (if insight-use-cursor-color
      (progn
        (setq insight-use-cursor-color nil)
        (setq insight-cursor-color-default nil)
        (remove-hook 'post-command-hook 'insight-check-cursor-color))
    (setq insight-cursor-color-default (face-attribute 'cursor :background))
    (setq insight-use-cursor-color t)
    (add-hook 'post-command-hook 'insight-check-cursor-color)))

(defun insight-switch-cursor-color (insight-mode)
  "Modify the color of the cursor when `insight-mode' is turned on

and `insight-use-cursor-color' is 't'."
  (when insight-use-cursor-color
    (if insight-mode
        (progn
          (setq-local insight-cursor-color-default-local (face-attribute 'cursor :background))
          (set-cursor-color insight-cursor-color))
      (set-cursor-color insight-cursor-color-default-local))))

;;; Tweaked scroll commands

(defun insight-scroll-down-line ()
  "Scroll down of one line"
  (interactive)
  (scroll-up-line -1))

(defun insight-scroll-up-line ()
  "Scroll up of one line"
  (interactive)
  (scroll-up-line))

(defun insight-scroll-other-window-up-line ()
  "Scroll up of one line in other window."
  (interactive)
  (scroll-other-window 1))

(defun insight-scroll-other-window-down-line ()
  "Scroll up of one line in other window."
  (interactive)
  (scroll-other-window-down 1))

(defun insight--half-window-height ()
  "Compute half window height."
  (/ (window-body-height) 2))

(defun insight-scroll-down-half-window ()
  "Scroll down of half window height."
  (interactive)
  (scroll-down (insight--half-window-height)))

(defun insight-scroll-up-half-window ()
  "Scroll up of half window height."
  (interactive)
  (scroll-up (insight--half-window-height)))

(defun insight-scroll-other-window-down-half-window ()
  "Scroll other window down of half window height."
  (interactive)
  (scroll-other-window-down (insight--half-window-height)))

(defun insight-scroll-other-window-up-half-window ()
  "Scroll other window up of half window of half window height."
  (interactive)
  (scroll-other-window (insight--half-window-height)))

;;; Scroll other window with `ace-window'

(declare-function ace-window "ext:ace-window")

(defun insight-ace-window ()
  "Interactively choose the displayed buffer to be scroll by commands
like `scroll-other-window' and `scroll-other-window-down'."
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'ace-window)
      (setq other-window-scroll-buffer (current-buffer)))
    (select-window initial-window)))

(defun insight-reset-other-window-scroll-buffer (&optional arg)
  "Reset `other-window-scroll-buffer' to nil if its value is not
a buffer being displayed.

Use this function to advice :before `scroll-other-window' and
`scroll-other-window-down'. This prevents to popup the buffer
`other-window-scroll-buffer' if it was not being displayed."
  (when (and other-window-scroll-buffer
             (or (not (eq 1 (length (get-buffer-window-list other-window-scroll-buffer))))
								 (eq (get-buffer-window other-window-scroll-buffer) (selected-window))))
    (setq other-window-scroll-buffer nil)))

(defun insight-set-window-advices ()
  "Advice `scroll-other-window' and `scroll-other-window-down'.

See `insight-ace-window' and
`insight-reset-other-window-scroll-buffer'."
  (interactive)
  (advice-add 'scroll-other-window :before 'insight-reset-other-window-scroll-buffer)
  (advice-add 'scroll-other-window-down :before 'insight-reset-other-window-scroll-buffer))

(defun insight-unset-window-advices ()
  "Advice `scroll-other-window' and `scroll-other-window-down'.

See `insight-ace-window' and
`insight-reset-other-window-scroll-buffer'."
  (interactive)
  (advice-remove 'scroll-other-window 'insight-reset-other-window-scroll-buffer)
  (advice-remove 'scroll-other-window-down 'insight-reset-other-window-scroll-buffer))

;;; narrow

(declare-function org-toggle-narrow-to-subtree "org")
(declare-function org-narrow-to-element "org")
(declare-function sp-narrow-to-sexp "ext:smartparens")

(defun insight-sp-toggle-narrow (arg)
  "Toggle between `widen' and `sp-narrow-to-sexp'."
  (interactive "P")
  (if (buffer-narrowed-p) (widen)
    (sp-narrow-to-sexp arg)))

;;; keymap

(defvar insight-mode-map
  (let ((map (make-sparse-keymap)))
    ;; outline commands
    (define-key map (kbd "p") 'outline-previous-visible-heading)
    (define-key map (kbd "n") 'outline-next-visible-heading)
    (define-key map (kbd "f") 'outline-forward-same-level)
    (define-key map (kbd "b") 'outline-backward-same-level)
    (define-key map (kbd "i") 'outline-up-heading)
    ;; scrolling commands
    (define-key map (kbd ")") 'scroll-left)
    (define-key map (kbd "(") 'scroll-right)
    (define-key map (kbd "<prior>") 'beginning-of-buffer)
    (define-key map (kbd "<next>") 'end-of-buffer)
    (define-key map (kbd "<backspace>") 'scroll-down)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "d") 'insight-scroll-down-half-window)
    (define-key map (kbd "s") 'insight-scroll-up-half-window)
    (define-key map (kbd "l") 'insight-scroll-down-line)
    (define-key map (kbd "r") 'insight-scroll-up-line)
		(define-key map (kbd "e") 'insight-scroll-other-window-down-half-window)
    (define-key map (kbd "o") 'insight-scroll-other-window-up-half-window)
		;; (define-key map (kbd "/") 'insight-scroll-other-window-down-line)
    ;; (define-key map (kbd "`") 'insight-scroll-other-window-up-line)
    (define-key map (kbd "a") 'insight-ace-window)
    ;; narrowing commands
    (define-key map (kbd ".") 'org-toggle-narrow-to-subtree)
    (define-key map (kbd ":") 'org-narrow-to-element)
    (define-key map (kbd ",") 'insight-sp-toggle-narrow)
    (define-key map (kbd ";") 'narrow-to-region)
    (define-key map (kbd "]") 'widen)
    ;; scaling commands
    (define-key map (kbd "+") 'text-scale-increase)
    (define-key map (kbd "-") 'text-scale-decrease)
    (define-key map (kbd "0") 'text-scale-adjust)
    ;; other
		(define-key map (kbd "q") 'insight-mode)
    map)
  "Keymap for `insight-mode'.")


;;; Mode

(define-minor-mode insight-mode
  "Convenient keybindings for zooming, scrolling and narrowing.

Mainly one key binding."
  :lighter " Insight"
  :keymap insight-mode-map
  (insight-switch-cursor-color insight-mode))

(insight-mode -1)

(provide 'insight)
