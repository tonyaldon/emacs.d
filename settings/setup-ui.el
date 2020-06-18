(set-face-attribute 'default nil :family "DejaVu Sans Mono")
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;; if screen size 1280x1024
;; (set-face-attribute 'default nil :height 160)
;; if screen size 1366x768
;; (set-face-attribute 'default nil :height 190)
;; to record video if screen size 1366x768
;; (set-face-attribute 'default nil :height 260)
;; to record video if screen size 1920x1080
;; (set-face-attribute 'default nil :height 260)

(setq inhibit-startup-screen t)
(setq frame-title-format
      '(buffer-file-name "%f"
                         (dired-directory dired-directory "%b")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode -1)
(setq linum-format " %7i ")
(setq-default cursor-type '(bar . 2))
(blink-cursor-mode -1)
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(setq frame-resize-pixelwise t)

(add-hook 'help-mode-hook (lambda () (visual-line-mode t)))
(setq
 display-buffer-alist
 '(("\\*Help.*"
    (display-buffer-in-side-window)
    (window-width . 0.3)
    (side . left)
    (slot . -1))
   ("\\*Messages.*\\|\\*Warnings.*\\|\\*Backtrace.*\\|*Apropos.*"
    (display-buffer-in-side-window)
    (window-width . 0.3)
    (side . left)
    (slot . 1))
	 ("\\*info.*"
    (display-buffer-in-side-window)
    (window-width . 0.36)
    (side . left)
    (slot . 1))
	 ("*scratch.*\\|*YASnippet Tables by NAMEHASH*\\|*YASnippet Tables*"
    (display-buffer-in-side-window)
    (window-width . 0.3)
    (side . left)
    (slot . 2))
	 ("\\*.*occur.*\\|\\*grep.*"
    (display-buffer-in-side-window)
		(window-height . 0.3)
		(side . top)
		(slot . 1))
	 ("\\*terminal.*"
    (display-buffer-in-side-window)
    (window-height . 0.25)
    (side . top)
    (slot . -1))
	 ("magit:.*\\|magit-diff:.*"
    (display-buffer-in-direction)
    (direction . left))
	 ("magit-log:.*"
    (display-buffer-below-selected))))


(provide 'setup-ui)
