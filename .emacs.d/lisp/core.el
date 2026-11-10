;;; core.el --- Core configuration -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Startup performance
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))


(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message ()
  (message nil))

(setq frame-title-format "Emacs")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'core)
