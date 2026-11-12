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

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :hook (zig-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls"))))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Compile to PDF by default
(setq TeX-PDF-mode t)

;; Enable SyncTeX (allows jumping between source and PDF)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)

(provide 'programming)
