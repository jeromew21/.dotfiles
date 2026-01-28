;; This is my emacs init file.
;; Some nice things to add would be:
;; - Font family cycling
;; - Make this thing idempotent so I don't have to keep restarting

;; (setq debug-on-error t)  ;; Uncomment this out to show backtrace on errors

;; Speed up startup
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Try speed up editor
(setq redisplay-skip-fontification-on-input t)  ;; Skip font-lock during fast typing
(setq bidi-inhibit-bpa t)  ;; Disable bidirectional text (if you don't need Arabic/Hebrew)
(setq-default bidi-display-reordering nil)
(setq jit-lock-defer-time 0.05)  ;; Defer syntax highlighting slightly

;; Write to custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Font setup
(defun my/font-installed-p (font-name)
  "Check if FONT-NAME is installed on the system."
  (member font-name (font-family-list)))

(defun my/get-first-available-font (font-list)
  "Return the first available font from FONT-LIST, or nil if none found."
  (seq-find #'my/font-installed-p font-list))

(defun my/set-font (font-name)
  "Set the font to FONT-NAME if available."
  (when (my/font-installed-p font-name)
    (set-face-attribute 'default nil
                        :family font-name
                        :height (* my-default-font-size 10))
    (message "Font set to: %s" font-name)
    t))

(defun my/cycle-font ()
  "Cycle through available fonts in my-font-list."
  (interactive)
  (let ((available-fonts (seq-filter #'my/font-installed-p my-font-list)))
    (if (null available-fonts)
        (message "No fonts from the list are installed!")
      (setq my-current-font-index
            (mod (1+ my-current-font-index) (length available-fonts)))
      (let ((next-font (nth my-current-font-index available-fonts)))
        (when (my/set-font next-font)
          ;; Update centaur-tabs font too
          (when (featurep 'centaur-tabs)
            (centaur-tabs-change-fonts next-font (* my-default-font-size 10))))))))

(defun my/increase-font-size ()
  "Increase font size by 1."
  (interactive)
  (setq my-default-font-size (+ my-default-font-size 1))
  (set-face-attribute 'default nil :height (* my-default-font-size 10))
  ;; Update centaur-tabs if loaded
  (when (featurep 'centaur-tabs)
    (centaur-tabs-change-fonts (face-attribute 'default :family)
                                (face-attribute 'default :height)))
  (message "Font size: %d" my-default-font-size))

(defun my/decrease-font-size ()
  "Decrease font size by 1."
  (interactive)
  (setq my-default-font-size (- my-default-font-size 1))
  (set-face-attribute 'default nil :height (* my-default-font-size 10))
  ;; Update centaur-tabs if loaded
  (when (featurep 'centaur-tabs)
    (centaur-tabs-change-fonts (face-attribute 'default :family)
                                (face-attribute 'default :height)))
  (message "Font size: %d" my-default-font-size))

(defun my/reset-font-size ()
  "Reset font size to default (12)."
  (interactive)
  (setq my-default-font-size 12)
  (set-face-attribute 'default nil :height (* my-default-font-size 10))
  (when (featurep 'centaur-tabs)
    (centaur-tabs-change-fonts (face-attribute 'default :family)
                                (face-attribute 'default :height)))
  (message "Font size reset to: %d" my-default-font-size))

(setq my-font-list
      '("JetBrainsMono Nerd Font"
        "Inconsolata Nerd Font"
        "Iosevka Nerd Font"
        "Comic Code"
        "ComicShannsMono Nerd Font"
        "FiraMono Nerd Font"
        "MesloLGS Nerd Font"))
(setq my-default-font-size 12)
(setq my-current-font-index 0)

(let ((initial-font (my/get-first-available-font my-font-list)))
  (if initial-font
      (my/set-font initial-font)
    (message "Warning: No fonts from my-font-list are installed!")))

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
(add-hook 'buffer-list-update-hook #'my/kill-buffer-if-file-deleted)

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
;; (use-package almost-mono-themes
;;   :config
;;     (load-theme 'almost-mono-gray t)
;;     (load-theme 'almost-mono-cream t)
;;     (load-theme 'almost-mono-white t)
;;     (load-theme 'almost-mono-black t)) ;; Less syntax highlighting, but easy on the eyes
;; (load-theme 'modus-vivendi-tinted t)

(use-package ef-themes ;; My endgame theme
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

;; Mode line
;; Try replacing doom-modeline with a simpler one:
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; Or just use the default modeline:
;; (doom-modeline-mode -1)

;; (use-package doom-modeline
;;   :ensure t
;;   :init
;;   (setq doom-modeline-height 1
;;         doom-modeline-bar-width 0
;;         doom-modeline-hud nil
;;         doom-modeline-icon nil
;;         doom-modeline-major-mode-icon nil
;;         doom-modeline-major-mode-color-icon nil
;;         doom-modeline-buffer-state-icon nil
;;         doom-modeline-buffer-modification-icon nil
;;         doom-modeline-enable-word-count nil
;;         doom-modeline-buffer-encoding nil
;;         doom-modeline-env-version nil
;;         doom-modeline-time nil
;;         doom-modeline-indent-info nil
;;         doom-modeline-lsp nil
;;         doom-modeline-github nil
;;         doom-modeline-github-interval 0
;;         doom-modeline-minor-modes nil
;;         doom-modeline-persp-name nil
;;         doom-modeline-workspace-name nil
;;         doom-modeline-checker-simple-format t
;;         doom-modeline-vcs-max-length 12)
;;   :config
;;   (doom-modeline-mode 1))

;; File explorer
(use-package treemacs
  :ensure t
  :config
  ;; Visual improvements
  :init
  (setq treemacs-width 30
        treemacs-indentation 2
        treemacs-no-png-images t
        treemacs-is-never-other-window t)
  :config
  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode -1)
  (add-hook 'treemacs-mode-hook
            (lambda () (display-line-numbers-mode -1))))

(set-face-attribute 'treemacs-root-face nil
                    :height 1.0
                    :weight 'normal
                    :foreground nil
                    :inherit 'default)

(use-package treemacs-evil)
(defun my/toggle-treemacs-focus ()
  "Toggle focus between Treemacs and the last window."
  (interactive)
  (if (string= (buffer-name) "*Treemacs-Framebuffer*")
      (other-window 1)
    (treemacs-select-window)))

;; [DISABLED] Project management
;; (use-package projectile
;;   :ensure t
;;   :config (projectile-mode 1))
;;  (with-eval-after-load 'evil
;;  (define-key evil-normal-state-map (kbd "<f5>") #'projectile-run-project))
;; (treemacs-project-follow-mode t)

;; Tabbed layout
(use-package centaur-tabs
  :demand
  :bind
  (("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("s-]" . 'centaur-tabs-forward)
  ("s-[" . 'centaur-tabs-backward))
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-change-fonts (face-attribute 'default :family) (face-attribute 'default :height))
  (centaur-tabs-headline-match))
(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "●")

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
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.05))

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
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-rename-use-prepare nil
        lsp-lens-enable nil))                 ; Disable code lens

;; Python support with Pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; C++ mode hooks
(add-hook 'c++-mode-hook
          (lambda ()
            (electric-indent-mode 1)
            (electric-pair-mode 1)
            (local-set-key (kbd "RET") 'c-context-line-break)))

;; Leader for evil
;; NOTE: Some of these commands may be hallucinated. I'm trying to remove them.
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
    "f +" 'my/increase-font-size
    "f -" 'my/decrease-font-size
    "f =" 'my/reset-font-size
    "f c" 'my/cycle-font)
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
