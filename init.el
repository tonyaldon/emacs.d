;;; Init packages

(require 'package)
(require 'cask "~/.cask/cask.el")

(cask-initialize)

;;; load-path

(add-to-list 'load-path "~/.emacs.d/settings/settings/")
(add-to-list 'load-path "~/.emacs.d/settings/utils/")
(add-to-list 'load-path "~/.emacs.d/settings/key-bindings/")
(add-to-list 'load-path "~/.emacs.d/settings/packages/")
(add-to-list 'load-path "~/.emacs.d/settings/packages/inside-emacs/")
(add-to-list 'load-path "~/.emacs.d/settings/packages/inside-emacs/story/")

(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'load-path "~/.emacs.d/packages/ac-html-csswatcher/")
(add-to-list 'load-path "~/.emacs.d/packages/company-emoji/")
(add-to-list 'load-path "~/.emacs.d/packages/company-web/")
(add-to-list 'load-path "~/.emacs.d/packages/peep-dired/")
(add-to-list 'load-path "~/.emacs.d/packages/fzf.el/")

;;; theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'discreet t)

;;; emacs-lisp utils

(require 'comment)

;;; setup files

(require 'setup-init)
(require 'setup-completion)
(require 'setup-dired)
(require 'setup-emacs-lisp-mode)
(require 'setup-ibuffer)
(require 'setup-org)
(require 'setup-python-mode)

;;; key bindings files

(require 'kb)
(require 'kb-lines)
(require 'kb-mark)
(require 'kb-smartparens)
(require 'kb-string)
(require 'kb-term)
(require 'kb-windows)
(require 'kb-outline)
