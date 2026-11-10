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

(use-package go-mode
  :ensure nil
  :hook ((go-mode . eglot-ensure)
         (go-mode . (lambda ()
                      (setq-local tab-width 4)
                      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))))

(provide 'programming)
