;; This is my emacs init file.
;; Some nice things to add would be:
;; - Font family cycling
;; - Make this thing idempotent so I don't have to keep restarting


;; Speed up startup
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Write to custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set font
(setq my-default-font "JetBrainsMono Nerd Font")
;; (setq my-default-font "Inconsolata Nerd Font")
;; (setq my-default-font "Iosevka Nerd Font")
;; (setq my-default-font "Comic Code")
;; (setq my-default-font "MesloLGS Nerd Font")
;; (setq my-default-font "FiraMono Nerd Font")
(setq my-default-font-size 12)
(set-face-attribute 'default nil
                    :family my-default-font
                    :height (* my-default-font-size 10))  ; Height is in 1/10pt

;; Disable UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq frame-title-format "Emacs")

;; Disable sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Disable tilde files
(setq make-backup-files nil)

;; Hide mouse
;; This is a bit flaky on Wayland, but it's good enough.
(setq make-pointer-invisible t)
(mouse-avoidance-mode 'banish)

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode)

;; Column limit indicator - 100 for now
(setq-default fill-column 100)
(global-display-fill-column-indicator-mode 1)

;; Auto-reload files changed externally
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil
      auto-revert-avoid-polling t
      auto-revert-interval 1)  ; Check every 1 second
(setq global-auto-revert-non-file-buffers t) ;; Also revert Dired buffers

;; Kill buffers for deleted files automatically
(defun my/kill-buffer-if-file-deleted ()
  "Kill buffer if its file has been deleted."
  (when (and buffer-file-name
             (not (file-exists-p buffer-file-name)))
    (kill-buffer (current-buffer))))

;; Set encoding (May be requirement on Windows only)
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
(global-whitespace-mode -1)

;; Save sessions
(desktop-save-mode 1)

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
  (load-theme 'almost-mono-black t)) ;; Less syntax highlighting, but easy on the eyes
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

;; File explorer
(use-package treemacs
  :ensure t
  :config
  ;; Visual improvements
  :init
  (setq treemacs-width 30
        treemacs-indentation 2
        treemacs-is-never-other-window t)
  :config
  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode -1)
  (add-hook 'treemacs-mode-hook
            (lambda () (display-line-numbers-mode -1))))
(setq treemacs-no-png-images t)
(with-eval-after-load 'treemacs
  (set-face-attribute 'treemacs-root-face nil
                      :height 1.0
                      :weight 'normal
                      :foreground nil  ; Use default foreground
                      :inherit 'default))
(use-package treemacs-evil)
(defun my/toggle-treemacs-focus ()
  "Toggle focus between Treemacs and the last window."
  (interactive)
  (if (string= (buffer-name) "*Treemacs-Framebuffer*")
      (other-window 1)
    (treemacs-select-window)))

;; [DISABLED] Project management
[
(use-package projectile
  :ensure t
  :config (projectile-mode 1))
 (with-eval-after-load 'evil
 (define-key evil-normal-state-map (kbd "<f5>") #'projectile-run-project))
(treemacs-project-follow-mode t)
]

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

;; Integrated terminal
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t)  ;; Auto-close buffer when shell exits
  (defun my/vterm ()
    "Open vterm in specific directory."
    (interactive)
    (let ((default-directory "~/"))
      (vterm)))
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local show-trailing-whitespace nil)
              (display-line-numbers-mode -1)
              (setq-local global-hl-line-mode nil))))  ;; Disable line highlighting too

;; Keybinding help
(use-package which-key
  :config (which-key-mode))

;; Company for autocompletion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;; LSP
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp)
         (c-mode . lsp)
         (python-mode . lsp))
  :config
  (setq lsp-keymap-prefix "C-c l"
        read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-headerline-breadcrumb-enable nil  ; Disable breadcrumb
        lsp-ui-sideline-enable nil            ; Disable sideline
        lsp-ui-doc-enable nil                 ; Disable hover doc popup
        lsp-auto-install-server t
        lsp-enable-file-watchers nil
        lsp-lens-enable nil))                 ; Disable code lens
(setq lsp-rename-use-prepare nil) ;; Force minibuffer rename

;; Python support with Pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; Leader for evil
;; NOTE: Some of these commands are hallucinated.
(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (add-hook 'treemacs-mode-hook
          (lambda ()
            (evil-leader-mode 1)
            (evil-normalize-keymaps))) ;; refresh evil maps
  (evil-leader/set-key "x" 'kill-current-buffer)
  (evil-leader/set-key "]" 'centaur-tabs-forward)
  (evil-leader/set-key "[" 'centaur-tabs-backward)
  (evil-leader/set-key "/" 'comment-line)
  (evil-leader/set-key "o" 'my/toggle-treemacs-focus)
  (evil-leader/set-key
    "l r" 'lsp-rename
    "l a" 'lsp-execute-code-action
    "l f" 'lsp-format-buffer
    "l o" 'lsp-organize-imports       ;; Organize imports (Python)
    "l R" 'lsp-workspace-restart)     ;; Restart LSP server
  (evil-leader/set-key "s g" 'rgrep)  ;; Grep in directory
  (evil-leader/set-key
    ;; Buffer navigation
    "b b" 'switch-to-buffer         ;; Interactive buffer switch
    "b n" 'next-buffer              ;; Next buffer
    "b p" 'previous-buffer          ;; Previous buffer
    "b d" 'kill-current-buffer      ;; Delete/close buffer
    "b k" 'kill-current-buffer      ;; Alternative (same as above)

    ;; Buffer management
    "b s" 'save-buffer              ;; Save current buffer
    "b r" 'revert-buffer            ;; Reload buffer from disk
    "b R" 'rename-buffer            ;; Rename buffer

    ;; Kill multiple
    "b D" 'kill-buffer-and-window)  ;; Kill buffer and close window
  (evil-leader/set-key "!" 'shell-command)
  (evil-leader/set-key
    "t t" 'my/vterm                    ;; If you have vterm installed
    "t e" 'eshell                   ;; Built-in Emacs shell
    "t s" 'shell)                   ;; Built-in shell
  (evil-leader/set-key "SPC" 'execute-extended-command)  ;; SPC SPC = M-x
  (evil-leader/set-key
    ;; Splits
    "w v" 'split-window-right       ;; Vertical split
    "w s" 'split-window-below       ;; Horizontal split
    "w d" 'delete-window            ;; Delete current window
    "w o" 'delete-other-windows     ;; Only keep current window
    ;; Navigation (alternative to Ctrl bindings)
    "w h" 'evil-window-left
    "w j" 'evil-window-down
    "w k" 'evil-window-up
    "w l" 'evil-window-right
    "w w" 'other-window             ;; Cycle through windows
    ;; Resizing
    "w =" 'balance-windows          ;; Make all windows equal size
    "w m" 'maximize-window          ;; Maximize current window (toggle)
    ;; Movement (move window itself)
    "w H" 'evil-window-move-far-left
    "w J" 'evil-window-move-very-bottom
    "w K" 'evil-window-move-very-top
    "w L" 'evil-window-move-far-right))

;; Evil mode
(use-package evil
  :ensure t
  :after evil-leader
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo))
(global-unset-key (kbd "C-r"))
