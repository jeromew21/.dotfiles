;; Add configuration directory to load-path
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; Load configuration modules
(require 'packages)
(require 'core)
(require 'appearance)
(require 'editing)
;;(require 'completion)
;;(require 'navigation)
(require 'programming)
(require 'keybindings)

;; Custom variables
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
