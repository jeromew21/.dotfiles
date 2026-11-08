;;; python.el --- Python configuration -*- lexical-binding: t; -*-

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-ts-mode . eglot-ensure)
  :config
  (setq python-indent-offset 4))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio"))))

(defun my/python-format-buffer ()
  (interactive)
  (call-process-region
   (point-min) (point-max)
   "ruff" t t nil
   "format" "--stdin-filename" buffer-file-name))

(provide 'py)
