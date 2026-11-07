;;; cpp.el --- C++ configuration -*- lexical-binding: t; -*-

;; Tree-sitter
(setq treesit-font-lock-level 4)

(add-to-list 'major-mode-remap-alist
             '(c++-mode . c++-ts-mode))

;; C++ editing
(add-hook 'c++-ts-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4)
            (electric-pair-mode 1)))

;; Eglot
(add-hook 'c++-ts-mode-hook #'eglot-ensure)

(setq eglot-ignored-server-capabilities
      '(:inlayHintProvider))

(setq-default c-ts-mode-indent-offset 4)

(provide 'cpp)
