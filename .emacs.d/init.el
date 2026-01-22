;;; This is my emacs init file.


;;; WINDOW SETUP


;; Speed up startup
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Write to custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set font
;; (setq my-default-font "Inconsolata Nerd Font")
;; (setq my-default-font "Iosevka Nerd Font")
(setq my-default-font "Comic Code")
(setq my-default-font-size 12)
(set-face-attribute 'default nil
                    :family my-default-font
                    :height (* my-default-font-size 10))  ; Height is in 1/10pt

;; Disable UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq frame-title-format nil)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Disable tilde files
(setq make-backup-files nil)

;; Hide mouse
(setq make-pointer-invisible t)
(mouse-avoidance-mode 'banish)

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode)

;; Set encoding (Possibly required on windows only)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)

;; Tabs are four spaces
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Scroll properties
(setq scroll-margin 3               ;; start scrolling before reaching the edge
      scroll-conservatively 101     ;; never recenter unless necessary
      scroll-step 1                 ;; scroll line-by-line
      scroll-preserve-screen-position t
      auto-window-vscroll nil)      ;; reduce CPU use for vertical scrolling

;; Require ending newlines
(setq require-final-newline t)
(setq-default show-trailing-whitespace t)

;; Save sessions
;; (desktop-save-mode 1)


;;; END WINDOW SETUP


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
[
(use-package almost-mono-themes
  :config
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
  (load-theme 'almost-mono-black t))
]

;; (load-theme 'modus-vivendi-tinted t)

(use-package ef-themes
  :config
  (load-theme 'ef-dark t))

;; Autosaving
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5)
  ;; Save all buffers, not just current
  (setq super-save-all-buffers t))

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
(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode -1)
  (add-hook 'treemacs-mode-hook
            (lambda () (display-line-numbers-mode -1)))
  ;; Visual improvements
  (setq treemacs-width 30
        treemacs-indentation 2
        treemacs-is-never-other-window t))
(setq treemacs-no-png-images t)
(with-eval-after-load 'treemacs
  (set-face-attribute 'treemacs-root-face nil
                      :height 1.0
                      :weight 'normal
                      :foreground nil  ; Use default foreground
                      :inherit 'default))

(use-package treemacs-evil)
(treemacs-project-follow-mode t) ;; if we do remove Projectile, what do we do with this?

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
(centaur-tabs-change-fonts my-default-font (* my-default-font-size 10))
(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "●")
(centaur-tabs-headline-match)

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

;; Auto completion
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

(global-unset-key (kbd "C-r"))
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo))
