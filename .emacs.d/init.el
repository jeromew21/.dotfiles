;;; init.el --- Root configuration -*- lexical-binding: t; -*-

;; Add configuration directory to load-path
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; Load configuration modules
(require 'core)
(require 'packages)
(require 'appearance)
(require 'theme)
(require 'editing)
(require 'debugging)
(require 'programming)
(require 'fonts)
(require 'keybindings)

;; Custom variables
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
