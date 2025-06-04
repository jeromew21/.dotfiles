;; Speed up startup
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(scroll-bar-mode -1)

(load-theme 'catppuccin t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)

(global-unset-key (kbd "C-r"))
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo))

;; Bind Command + ] to centaur-tabs-forward (instead of C-<next>)
(global-set-key (kbd "s-]") 'centaur-tabs-forward)

;; Bind Command + [ to centaur-tabs-backward (instead of C-<prior>)
(global-set-key (kbd "s-[") 'centaur-tabs-backward)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq frame-title-format nil)

(setq scroll-margin 3                ;; start scrolling before reaching the edge
      scroll-conservatively 101     ;; never recenter unless necessary
      scroll-step 1                 ;; scroll line-by-line
      scroll-preserve-screen-position t
      auto-window-vscroll nil)      ;; reduce CPU use for vertical scrolling

;; Require ending newlines
(setq require-final-newline t)
(setq-default indicate-empty-lines t)
(setq-default mode-require-final-newline t)
;; optional
(when (fboundp 'toggle-indicate-empty-lines)
  (toggle-indicate-empty-lines 1))
(setq whitespace-style '(face trailing tabs newline empty))
(global-whitespace-mode 1)

(desktop-save-mode 1)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(tool-bar-mode -1)

(set-frame-font "Inconsolata Nerd Font 18" nil t)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)  ;; indentation width
  (setq tab-width 4)       ;; visual tab width
  (setq indent-tabs-mode nil)) ;; use spaces instead of tabs

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)

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

;; Setup undo-redo
(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil))  ;; Optional: avoid clutter
(setq evil-undo-system 'undo-tree)

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 1
        doom-modeline-bar-width 0
        doom-modeline-hud nil
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-env-version nil
        doom-modeline-time nil
        doom-modeline-indent-info nil
        doom-modeline-lsp nil
        doom-modeline-github nil
        doom-modeline-github-interval 0
        doom-modeline-minor-modes nil
        doom-modeline-persp-name nil
        doom-modeline-workspace-name nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12)
  :config
  (doom-modeline-mode 1))

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
  (setq lsp-enable-snippet t)
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-semantic-tokens-enable t))
  
;; LSP UI
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-enable t))

;; Catpuccin theme
(use-package catppuccin-theme
  :init (setq catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm)
  (catppuccin-reload))
  
;; Treemacs integration with LSP
(use-package lsp-treemacs
  :after lsp)
  
;; DAP mode (Debugging)
;; TODO fix it (might just not work outside of Linux)

;; imenu
(use-package imenu-list
  :ensure t
  :bind ("<f7>" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t))
  
;; File explorer
(use-package treemacs)
(global-set-key [f8] 'treemacs)
(setq treemacs-no-png-images t)  ;; If icons aren't showing properly
(use-package treemacs-evil)
(treemacs-project-follow-mode t)

;; Tabbed layout
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; Minimap mode
(use-package minimap
  :config
  (setq minimap-window-location 'right
        minimap-width-fraction 0.1))
  
;; Git integration
(use-package magit)
  
;; CMake mode
(use-package cmake-mode)
  
;; Clang-format integration
(use-package clang-format
  :hook ((c-mode . (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t)))
         (c++-mode . (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t))))
  :config (setq clang-format-style-option "llvm"))

;; vterm integrated terminal
(use-package vterm
  :ensure t
  :bind
  ("<f9>" . vterm))
  
;; Keybinding help
(use-package which-key
  :config (which-key-mode))
  
;; Linting
(use-package flycheck
 :init (global-flycheck-mode))
 
;; Evil mode 
(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo))

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
