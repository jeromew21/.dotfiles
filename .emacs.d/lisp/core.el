;;; core.el --- Core configuration -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Startup performance
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(setq frame-title-format "Emacs")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Encoding
(prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-language-environment 'utf-8)

;; Backups
(setq make-backup-files nil
      auto-save-default nil)

;; Save all modified files when Emacs loses focus
(add-hook 'focus-out-hook
          (lambda ()
            (save-some-buffers t)))

(provide 'core)
