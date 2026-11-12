;;; core.el --- Core configuration -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message ()
  (message nil))

(provide 'core)
