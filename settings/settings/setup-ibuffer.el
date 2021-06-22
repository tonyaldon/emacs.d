;;; Packages

(require 'ibuffer)

(defalias 'list-buffers 'ibuffer)

(setq ibuffer-expert t)
(setq ibuffer-use-header-line nil)
(setq ibuffer-formats
      '((mark
         modified " "
         (name 28 28 :left :elide) " "
         filename-and-process)))

(defun ta-ibuffer-preview ()
  "Preview buffer at `point'."
  (interactive)
  (when-let ((buffer-at-point (ibuffer-current-buffer t)))
    (if (eq 2 (length (window-list)))
        (display-buffer buffer-at-point t)
      (display-buffer buffer-at-point
                      '(display-buffer-in-direction
                        (direction . right)
                        (window-width . 0.74))))))

(defun ta-ibuffer-previous ()
  "Preview buffer on the previous line."
  (interactive)
  (ibuffer-backward-line)
  (ta-ibuffer-preview))

(defun ta-ibuffer-next ()
  "Preview buffer on the next line."
  (interactive)
  (ibuffer-forward-line)
  (ta-ibuffer-preview))

(define-key ibuffer-mode-map (kbd "M-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)
(define-key ibuffer-mode-map (kbd "p") 'ta-ibuffer-previous)
(define-key ibuffer-mode-map (kbd "n") 'ta-ibuffer-next)
(define-key ibuffer-mode-map (kbd "M-s") 'isearch-forward)

;;; Footer

(provide 'setup-ibuffer)
