(require 'hydra)
(require 'key-chord)
(require 'iso-transl)

(setq-default hydra-hint-display-type  'message)

(key-chord-mode t)
(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.2)

(defun hydra-color-pre ()
  "Here, the used color matches with d-green-2 defined in `discreet-theme'"
	(set-cursor-color "#26f9ad"))

(defun hydra-color-post ()
  "Here, the used color matches with d-green-2 defined in `discreet-theme.'"
	(set-cursor-color "#26f9ad"))

(defun hydra-color-pre-scrolling ()
  "Here, the used color matches with d-white-1 defined in `discreet-theme'"
	(set-cursor-color "#dedede"))

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

(defun hydra-color-pre-org ()
  "Here, the used color matches with d-white-0 defined in `discreet-theme'"
	(set-cursor-color "#ffffff"))

(defun hydra-color-pre-sgml ()
  "Here, the used color matches with d-white-0 defined in `discreet-theme'"
	(set-cursor-color "#ffffff"))

(defun hydra-color-pre-mc ()
  "Here, the used color matches with d-blue-1 defined in `discreet-theme'"
	(set-cursor-color "#87cefa"))



(define-key key-translation-map (kbd "M-q") (kbd "C-g"))
(global-set-key [escape] 'kill-this-buffer)
(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "M-+") 'undo-redo)
(global-set-key (kbd "<f2>") 'kill-ring-save)
(global-set-key (kbd "<f3>") 'kill-region)
(global-set-key (kbd "<f4>") 'yank)

;; (key-chord-define-global "sr" 'ert)
(global-set-key (kbd "<f1>") 'eval-defun)
(key-chord-define-global "pf" 'eval-last-sexp)


(provide 'kb)
