;; Specifically, to silence warning about package-initialize
(setq warning-minimum-level :emergency)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(add-to-list 'load-path "~/.emacs.d/settings/")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/plugins/php-extras/")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'discreet t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'setup-ui)
(require 'setup-mode-line)
(require 'setup-dired)
(require 'setup-ibuffer)

(require 'setup-init)
(require 'setup-yasnippet)
(require 'setup-company)
(require 'setup-emmet-mode)
(require 'setup-flycheck)

(require 'setup-php-mode)
(require 'setup-sql-mode)
(require 'setup-js-mode)
(require 'setup-html-mode)
(require 'setup-css-mode)
(require 'setup-python-mode)
(require 'setup-emacs-lisp-mode)
;;(require 'setup-latex-mode)
(require 'setup-gmake-makefile-mode)
(require 'setup-sgml-mode)


(require 'kb)
(require 'kb-company)
(require 'kb-describe)
(require 'kb-files)
(require 'kb-lines)
(require 'kb-kmacro)
(require 'kb-mark)
(require 'kb-narrow)
(require 'kb-org)
(require 'kb-scrolling)
(require 'kb-search)
(require 'kb-smartparens)
(require 'kb-string)
(require 'kb-sgml)
(require 'kb-term)
(require 'kb-windows)


(require 'util-learning)
(require 'util-mathstyle)
(require 'util-writing)
