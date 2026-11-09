;;; programming.el --- Language settings -*- lexical-binding: t; -*-

(require 'cpp)
(require 'rust)
(require 'py)

(use-package elisp-autofmt :ensure t :commands (elisp-autofmt-buffer elisp-autofmt-mode))

(use-package eg-java
  :ensure nil
  :hook
  ((java-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure)))

(provide 'programming)
