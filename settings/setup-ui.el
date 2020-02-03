(require 'ivy-posframe)

;; if screen size 1280x1024
(set-face-attribute 'default nil :height 160)
;; if screen size 1366x768
;; (set-face-attribute 'default nil :height 190)
;; to record video if screen size 1366x768
;; (set-face-attribute 'default nil :height 260)

(setq inhibit-startup-screen t)
(setq frame-title-format
      '(buffer-file-name "%f"
                         (dired-directory dired-directory "%b")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode -1)

(setq-default cursor-type '(bar . 2))
(blink-cursor-mode -1)
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)

(defun ta-posframe-poshandler-frame-below-top-center (info)
  "Posframe's position handler.

structure of INFO can be found in docstring of `posframe-show'."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        180))

(defun ta-ivy-posframe-display-at-frame-below-top-center (str)
  (ivy-posframe--display str #'ta-posframe-poshandler-frame-below-top-center))

(setq ivy-posframe-display-functions-alist
      '((swiper . nil)
				(swiper-thing-at-point . nil)
        (t . ta-ivy-posframe-display-at-frame-below-top-center)))

(setq ivy-height 11)
(setq ivy-posframe-height 11)
(add-hook 'ivy-mode-hook 'ivy-posframe-enable)


(ivy-posframe-mode 1)


(provide 'setup-ui)
