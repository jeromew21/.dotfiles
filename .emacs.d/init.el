;;; init.el --- Root configuration -*- lexical-binding: t; -*-

(setq use-package-compute-statistics t)

;; Startup performance
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;; Reset after startup so typing stays smooth
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024))
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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
