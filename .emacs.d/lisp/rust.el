;;; rust.el --- Rust configuration -*- lexical-binding: t; -*-

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (setq rust-ts-mode-indent-offset 4))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"))))

(provide 'rust)
