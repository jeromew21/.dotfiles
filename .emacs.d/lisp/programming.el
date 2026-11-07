;;; programming.el --- Language settings -*- lexical-binding: t; -*-

(require 'cpp)
;; (require 'python)

(use-package elisp-autofmt
  :ensure t
  :commands (elisp-autofmt-buffer elisp-autofmt-mode)
)

(provide 'programming)
