(require 'hydra)
(require 'key-chord)
(require 'iso-transl)
(require 'avy)

(key-chord-mode t)

(setq-default hydra-hint-display-type  'message)
(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.2)
(setq avy-highlight-first t)
(setq avy-keys '(97 117 105 101 121 120 111 119 98 110 102 112 107 116 115
										114 113 100 108 106 109 99 103 104 40 41 91 93 60 62 44
										59 46 58))
(setq avy-style 'at-full)


(defun hydra-color-pre ()
  "Here, the used color matches with d-green-2 defined in `discreet-theme'"
	(set-cursor-color "#26f9ad"))

(defun hydra-color-post ()
  "Here, the used color matches with d-green-2 defined in `discreet-theme.'"
	(set-cursor-color "#26f9ad"))

(defun hydra-color-pre-browse ()
  "Here, the used color matches with d-white-1 defined in `discreet-theme'"
	(set-cursor-color "#ffffff"))

(defun hydra-color-pre-windows ()
  "Here, the used color matches with d-yellow-1 defined in `discreet-theme'"
	(set-cursor-color "#ffd500"))

(defun hydra-color-pre-lines ()
  "Here, the used color matches with d-pink-1 defined in `discreet-theme'"
	(set-cursor-color "#fa87ce"))

(defun hydra-color-pre-sp ()
  "Here, the used color matches with d-yellow-1 defined in `discreet-theme'"
	(set-cursor-color "#f92672"))

(defun hydra-color-pre-sp-reshape ()
  "Here, the used color matches with d-blue-1 defined in `discreet-theme'"
	(set-cursor-color "#fd721f"))

(defun hydra-color-pre-sgml ()
  "Here, the used color matches with d-white-0 defined in `discreet-theme'"
	(set-cursor-color "#02eaf3"))

(defun hydra-color-pre-mc ()
  "Here, the used color matches with d-blue-1 defined in `discreet-theme'"
	(set-cursor-color "#87cefa"))


(declare-function 'counsel-M-x "ext:counsel")

(define-key key-translation-map (kbd "M-q") (kbd "C-g"))
(global-set-key [escape] 'kill-this-buffer)
(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "M-+") 'undo-redo)
(global-set-key (kbd "<f2>") 'kill-ring-save)
(global-set-key (kbd "<f3>") 'kill-region)
(global-set-key (kbd "<f4>") 'yank)
(global-set-key (kbd "<f6>") 'save-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<right>") 'next-buffer)
(key-chord-define-global ";;" 'counsel-M-x)
(global-set-key (kbd "<f1>") 'eval-defun)
(key-chord-define-global "pf" 'eval-last-sexp)
(global-set-key (kbd "<C-backspace>") 'delete-char)


(provide 'kb)
