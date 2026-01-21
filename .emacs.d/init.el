;;; This is my emacs init file.

;;;
;;; WINDOW SETUP
;;;

;; Speed up startup
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Disable UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq frame-title-format nil)

;; Hide mouse
(setq make-pointer-invisible t)
(mouse-avoidance-mode 'banish)

;; Set font
;; (set-frame-font "Inconsolata Nerd Font 12" nil t)
;; (setq my-default-font "Iosevka Nerd Font")
(setq my-default-font "Comic Code")
(set-frame-font my-default-font nil t)

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Set encoding (Possibly windows required only)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)

;; Auto save
(defun my/save-buffer-if-needed ()
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))
(run-with-idle-timer 5 t #'my/save-buffer-if-needed)

;; Bind C-r to redo
(global-unset-key (kbd "C-r"))
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo))

;; Tabs are four spaces
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Scroll properties
(setq scroll-margin 3                ;; start scrolling before reaching the edge
      scroll-conservatively 101     ;; never recenter unless necessary
      scroll-step 1                 ;; scroll line-by-line
      scroll-preserve-screen-position t
      auto-window-vscroll nil)      ;; reduce CPU use for vertical scrolling

;; Require ending newlines
(setq require-final-newline t)
(setq-default indicate-empty-lines t)
(setq-default mode-require-final-newline t)
(when (fboundp 'toggle-indicate-empty-lines)
  (toggle-indicate-empty-lines 1))
(setq whitespace-style '(face trailing tabs newline empty))
(global-whitespace-mode 1)

;; Save sessions
(desktop-save-mode 1)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)


;;;
;;; END WINDOW SETUP
;;;


;; Set up required packages
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

;; Setup theme
(use-package almost-mono-themes
  :config
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
  (load-theme 'almost-mono-black t))

;; Reduce modeline information overload
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
  :ensure t
  :config (projectile-mode 1))

(with-eval-after-load 'evil
 (define-key evil-normal-state-map (kbd "<f5>") #'projectile-run-project))

;; File explorer
(use-package treemacs)
(setq treemacs-no-png-images t)  ;; If icons aren't showing properly
(use-package treemacs-evil)
(treemacs-project-follow-mode t)

(defun my/toggle-treemacs-focus ()
  "Toggle focus between Treemacs and the last window."
  (interactive)
  (if (string= (buffer-name) "*Treemacs-Framebuffer*")
      (other-window 1)
    (treemacs-select-window)))

;; Tabbed layout
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))
(global-set-key (kbd "s-]") 'centaur-tabs-forward)
(global-set-key (kbd "s-[") 'centaur-tabs-backward)
(centaur-tabs-change-fonts my-default-font 120)
(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "●")

;; Customize the tab faces
(centaur-tabs-headline-match)
;; vterm integrated terminal
(use-package vterm
  :ensure t
  :bind
  ("<f9>" . vterm))

;; Keybinding help
(use-package which-key
  :config (which-key-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :hook (c++-mode . lsp)
  :config
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable nil  ; Disable breadcrumb
        lsp-ui-sideline-enable nil            ; Disable sideline
        lsp-ui-doc-enable nil                 ; Disable hover doc popup
        lsp-lens-enable nil))                 ; Disable code lens

(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

;; Leader for evil
(use-package evil-leader
  :init
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (add-hook 'treemacs-mode-hook
          (lambda ()
            (evil-leader-mode 1)
            (evil-normalize-keymaps)))  ;; refresh evil maps
  (evil-leader/set-key "x" 'kill-buffer)
  (evil-leader/set-key "]" 'centaur-tabs-forward)
  (evil-leader/set-key "[" 'centaur-tabs-backward)
  (evil-leader/set-key "o" 'my/toggle-treemacs-focus))

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
