;; This is my emacs init file.
;; Some nice things to add would be:
;; - evil-leader-mode after evaluating this

;; Uncomment this out to show backtrace on errors
;; (setq debug-on-error t)

;; Speed up startup
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;; Rendering optimizations
(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      redisplay-dont-pause t
      inhibit-compacting-font-caches t
      bidi-inhibit-bpa t
      jit-lock-defer-time 0.05
      jit-lock-stealth-time nil)
(setq-default bidi-display-reordering nil)  ;; Buffer-local, needs setq-default

;; Disable UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq frame-title-format
      (format "Emacs %d.%d - %%b" emacs-major-version emacs-minor-version))

;; Disable colliding bindings
(global-unset-key (kbd "C-r"))

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

;; NOTE: these are ranked based on my preference.
;; TODO: save on exit? Right now we always start up with the first one.
(setq my-font-list
      '("JetBrainsMono Nerd Font" ;; Current favorite, all round good.
        "BlexMono Nerd Font" ;; Really solid, no notes
        "DepartureMono Nerd Font" ;; Nice retro font, very readable but not ugly. Maybe try disabling AA?
        "Comic Code" ;; Good font, but the line height seems wrong on this one on Emacs.
        "Inconsolata Nerd Font" ;; Another solid all-rounder. Weirdly, it's smaller than the others
        "Iosevka Nerd Font" ;; Efficient, skinny, iconic. The Tsoding font.
        "FiraCode Nerd Font" ;; Iconic, sharp. Feels almost like getting stabbed to read. Slightly wide kerning. Very nice on most screens. The Primagen font.
        "0xProto Nerd Font" ;; Readable, similar to Cascadia Code but prettier.
        "CodeNewRoman Nerd Font" ;; Smaller side, nice font. Very unprovocative. Familiar, like I've seen it somewhere before.
        "Cousine Nerd Font" ;; Similar to Plex Mono, nothing special
        "AtkynsonMono Nerd Font" ;; Similar to Source Code Pro and Inconsolata
        "IntoneMono Nerd Font" ;; Similar to Code New Roman. Unprovocative.
        "RobotoMono Nerd Font" ;; Readable, similar to JetBrains Mono
        "Noto Nerd Font" ;; Nice and similar to Plex Mono
        "Lilex Nerd Font" ;; Nice, I like this one, it's somewhere in between Bitstream Vera and Cascadia
        "DroidSansM Nerd Font" ;; Boring, reminds me of Lucida Console.
        "EnvyCodeR Nerd Font" ;; Probably not going to keep around, but it's interesting.
        "CommitMono Nerd Font" ;; Very round, it's interesting.
        "BitstromWera Nerd Font" ;; Classic, very boring. Like Fira Code but not pointy.
        "AnonymicePro Nerd Font" ;; Interesting, similar to Inconsolata but spicier.
        "AdwaitaMono Nerd Font" ;; Not exciting at all.
        "GeistMono Nerd Font" ;; Very round and slightly weird, but fine.
        "MesloLGS Nerd Font" ;; How is this different from Bitstream Vera?
        "ComicShannsMono Nerd Font" ;; Inferior to Comic Code
        "SauceCodePro Nerd Font" ;; I don't like it.
        "CaskaydiaMono Nerd Font"))
(setq my-default-font-size 12)
(setq my-current-font-index 0)

(let ((initial-font (my/get-first-available-font my-font-list)))
  (if initial-font
      (my/set-font initial-font)
    (message "Warning: No fonts from my-font-list are installed!")))

;; Write to custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable backup and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Hide mouse
;; This is a bit flaky on Wayland, but it's good enough.
(setq make-pointer-invisible t)
(mouse-avoidance-mode 'banish)

;; Line numbers
;; The argument against relative line numbers is:
;; - If yanking multiple lines, you have to add one to the number
;; - Too much motion on the screen
;; - May be a bottleneck
(setq display-line-numbers-type t)
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode)

;; Column limit indicator - 100 characters for now
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

;; Set encoding (NOTE: required on Windows only)
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

;; Markdown mode don't use alternative fonts
;; FIXME: it doesn't change when fonts get cycled
(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-code-face nil
                      :family (face-attribute 'default :family)
                      :height (face-attribute 'default :height))
  (set-face-attribute 'markdown-inline-code-face nil
                      :family (face-attribute 'default :family)
                      :height (face-attribute 'default :height))
  (set-face-attribute 'markdown-pre-face nil
                      :family (face-attribute 'default :family)
                      :height (face-attribute 'default :height)))

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
;; Themes are set in pairs, my-light-theme and my-dark-theme.
;; NOTE: evaluate and/or my/toggle-theme if new theme doesn't load properly.
(use-package solarized-theme
  :ensure t)
;; (use-package ef-themes
;;   :ensure t)
;; (use-package almost-mono-themes
;;   :ensure t)

(setq my-current-theme-is-dark nil)
(setq my-theme-state-file (concat user-emacs-directory "theme-state.el"))

(setq my-light-theme 'solarized-light)
(setq my-dark-theme 'solarized-dark)

;; (setq my-light-theme 'ef-light)
;; (setq my-dark-theme 'ef-dark)

;; (setq my-light-theme 'modus-operandi-tinted)
;; (setq my-dark-theme 'modus-vivendi-tinted)

;; (setq my-light-theme 'almost-mono-white)
;; (setq my-dark-theme 'almost-mono-black)

;; (setq my-light-theme 'almost-mono-cream)
;; (setq my-dark-theme 'almost-mono-gray)

;; Load saved theme state
(defun my/load-theme-state ()
  "Load theme state from file."
  (when (file-exists-p my-theme-state-file)
    (load my-theme-state-file)))

;; Save theme state
(defun my/save-theme-state ()
  "Save current theme state to file."
  (with-temp-file my-theme-state-file
    (prin1 `(setq my-current-theme-is-dark ,my-current-theme-is-dark) (current-buffer))))

;; Toggle function with auto-save
(defun my/toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (if my-current-theme-is-dark
      (progn
        (load-theme my-light-theme t)
        (setq my-current-theme-is-dark nil)
        (message "Switched to light theme: %s" my-light-theme))
    (progn
      (load-theme my-dark-theme t)
      (setq my-current-theme-is-dark t)
      (message "Switched to dark theme: %s" my-dark-theme)))
  (my/save-theme-state))  ;; Auto-save after toggle

;; Load state and apply theme on startup
(my/load-theme-state)
(if (bound-and-true-p my-current-theme-is-dark)
    (load-theme my-dark-theme t)
  (load-theme my-light-theme t))

;; Autosaving
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  ;; (setq super-save-auto-save-when-idle t
  ;;       super-save-idle-duration 5)
  (setq super-save-all-buffers t))

;; Mode line
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; File explorer
(use-package treemacs
  :ensure t
  :init
  (setq treemacs-indentation 2
        treemacs-no-png-images t
        ;; treemacs-width 40
        treemacs-is-never-other-window t)
  :config
  (set-face-attribute 'treemacs-root-face nil
        :height 1.0
        :weight 'normal
        :foreground 'unspecified
        :inherit 'default)
  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode -1)
  (add-hook 'treemacs-mode-hook
            (lambda () (display-line-numbers-mode -1))))

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
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))

;; Yasnippets for autocompletion
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Trying out this package
(use-package doxymacs
  :ensure t
  :hook (c++-mode . doxymacs-mode)
  :config
  (defun my/doxygen-comment ()
    "Insert Doxygen comment template."
    (interactive)
    (doxymacs-insert-function-comment)))

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

;; Python support with Pyright and Black
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config
  (setq lsp-pyright-use-library-code-for-types t)
  ;; Use black for formatting
  (setq lsp-pyright-python-executable-cmd "python3"))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))  ;; Auto-format on save

;; C++ mode hooks
(add-hook 'c++-mode-hook
          (lambda ()
            (electric-indent-mode 1)
            (electric-pair-mode 1)
            (local-set-key (kbd "RET") 'c-context-line-break)))

;; Kill ring
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

;; Leader for evil
;; NOTE: when evaluating this buffer, afterwards, execute M-x evil-leader-mode
(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "x f" 'find-file
    "x k" 'kill-current-buffer)
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
  (evil-leader/set-key "y" 'browse-kill-ring)
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
    "b l" 'list-buffers             ;; List buffers

    ;; Kill multiple
    "b D" 'kill-buffer-and-window)  ;; Kill buffer and close window
  (evil-leader/set-key "!" 'shell-command)
  (evil-leader/set-key
    "t t" 'my/vterm                    ;; If you have vterm installed
    ;; "t e" 'eshell                   ;; Built-in Emacs shell
    "t s" 'shell)                   ;; Built-in shell
  (evil-leader/set-key "SPC" 'execute-extended-command)  ;; SPC SPC = M-x
  (evil-leader/set-key
    ;; Appearance
    "f t" 'my/toggle-theme
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

(use-package treemacs-evil)
(defun my/toggle-treemacs-focus ()
  "Toggle focus between Treemacs and the last window."
  (interactive)
  (if (string= (buffer-name) "*Treemacs-Framebuffer*")
      (other-window 1)
    (treemacs-select-window)))

;; Use small cursor in terminal mode
(use-package evil-terminal-cursor-changer
  :ensure t
  :if (not (display-graphic-p))
  :config
  (evil-terminal-cursor-changer-activate)
  (setq evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-emacs-state-cursor  'hbar))
