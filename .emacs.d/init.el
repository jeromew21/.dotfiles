;; Speed up startup
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(tool-bar-mode -1)

(require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Project management
(use-package projectile
  :config (projectile-mode 1))

;; Auto-completion
(use-package company
  :config (global-company-mode))
  
;; Snippets
(use-package yasnippet
  :config (yas-global-mode 1))
  
;; LSP support
(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (c-mode . lsp))
  :commands lsp
  :config
  (setq lsp-prefer-capf t)
  (setq lsp-enable-snippet t))
  
;; LSP UI
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-enable t))
  
;; Treemacs integration with LSP
(use-package lsp-treemacs
  :after lsp)
  
;; DAP mode (Debugging)
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-lldb))
  
;; File explorer
(use-package treemacs)
  
;; Git integration
(use-package magit)
  
;; CMake mode
(use-package cmake-mode)
  
;; Clang-format integration
(use-package clang-format
  :hook ((c-mode . (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t)))
         (c++-mode . (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t))))
  :config (setq clang-format-style-option "llvm"))
  
;; Keybinding help
(use-package which-key
  :config (which-key-mode))
  
;; Linting
(use-package flycheck
 :init (global-flycheck-mode))

;; Catpuccin theme
(use-package catppuccin-theme)
(load-theme 'catppuccin :no-confirm)

;; Optional: Vim keybindings
(use-package evil
  :config (evil-mode 1))
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
