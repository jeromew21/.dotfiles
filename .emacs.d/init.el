;;; init.el --- Root configuration -*- lexical-binding: t; -*-

(setq use-package-compute-statistics t)

;; Startup performance (put in early-init.el if you have one, or top of init.el)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 4 1024 1024))

;; Reset after startup + desktop-read finish
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024))
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)));; Add configuration directory to load-path

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
