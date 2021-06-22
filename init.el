;;; Init packages

(require 'package)
(require 'cask "~/.cask/cask.el")

(cask-initialize)

;;; load-path

(add-to-list 'load-path "~/.emacs.d/settings/settings/")
(add-to-list 'load-path "~/.emacs.d/settings/key-bindings/")

(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'load-path "~/.emacs.d/packages/fzf.el/")
(add-to-list 'load-path "~/.emacs.d/packages/inside-emacs-tooling/")
(add-to-list 'load-path "~/.emacs.d/packages/inside-emacs-tooling/kdenlive/")
(add-to-list 'load-path "~/.emacs.d/packages/inside-emacs-tooling/ie-story/")

;;; theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'discreet t)

;;; emacs-lisp utils

(require 'comment)

;;; setup files

(require 'setup-init)
(require 'setup-completion)
(require 'setup-ibuffer)
(require 'setup-org)
(require 'setup-inside-emacs)

;;; key bindings files

(require 'kb)
(require 'kb-mark)
(require 'kb-string)
(require 'kb-windows)
