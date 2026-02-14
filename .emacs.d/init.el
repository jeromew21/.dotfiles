;; This is my emacs init file.
;; NOTE: reload this with my/reload-init
;; TODO: disable bolding

(require 'cl-lib)

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
(setq my-font-list
      '("JetBrains Mono"            ;; (10/10) Current favorite, all round good.
        "RecMonoCasual Nerd Font"   ;; ( 9/10) Goofy version of Rec Mono. This might be the one.
        "RecMonoLinear Nerd Font"   ;; ( 9/10) Nice font, like Fira but better.
        "0xProto Nerd Font"         ;; ( 9/10) Dyslexic font vibes. Readable, very high x-height, like a better Cascadia Code.
        "FantasqueSansM Nerd Font"  ;; ( 9/10) Small, but nice goofy font.
        "Sudo Var"                  ;; ( 9/10) Like Iosevka but prettier
        "M+1Code Nerd Font"         ;; ( 9/10) Slightly villainous vibes. Similar to Sudo Var but pointier. Looks better big.
        "IBM Plex Mono"             ;; ( 9/10) Really solid, no notes
        "Inconsolata Nerd Font"     ;; ( 9/10) Another solid all-rounder. Weirdly, it's smaller than the others.
        "Comic Code"                ;; ( 9/10) Good font, but the line height seems wrong on this one on Emacs.
        "Fira Code"                 ;; ( 8/10) Iconic, sharp, wide, to pointy. Very nice on most screens. The Primagen font.
        "CommitMono Nerd Font"      ;; ( 8/10) Very round and with serifs, it's interesting. Similar to Inconsolata.
        "VictorMono Nerd Font"      ;; ( 8/10) Futuristic and thin, nice.
        "JuliaMono"                 ;; ( 8/10) Pretty good. Like 0xProto and Fira but less going on.
        "DepartureMono Nerd Font"   ;; ( 8/10) Nice retro font, very readable but not ugly. Maybe try disabling AA?
        "Maple Mono NL"             ;; ( 8/10) Cutesy, Japanese vibes.
        "Noto Sans Mono"        ;; ( 7/10) Boring, inoffensive. New version of Droid Sans Mono.
        "Terminus"                  ;; ( 7/10) Nice retro font.
        "ProFont IIx Nerd Font"     ;; ( 7/10) Simple, retro-futuristic
        "Iosevka Nerd Font"         ;; ( 7/10) Efficient, skinny, iconic. The Tsoding font.
        "Cousine Nerd Font"         ;; ( 7/10) Highly familiar looking font. Identical as Liberation Mono.
        "Hurmit Nerd Font"          ;; ( 7/10) Unique, Reads okay
        "BigBlueTerm437 Nerd Font"  ;; ( 7/10) I think this is the GRUB font. It's iconic
        "GeistMono Nerd Font"       ;; ( 7/10) Very round and slightly weird, but fine.
        "SF Mono"                   ;; ( 7/10) Solid, similar to JetBrains mono, worse numbers.
        "Overpass Mono"             ;; ( 7/10) Nerd font version is broken
        "RobotoMono Nerd Font"      ;; ( 7/10) Readable, similar to JetBrains Mono. A bit boring ff.
        "Monaco"                    ;; ( 7/10) Iconic, boring. Lucida Console?
        "MonaspiceAr Nerd Font"     ;; ( 7/10) High contrast and readable.
        "MonaspiceKr Nerd Font"     ;; ( 7/10) Sharp variant, high contrast and readable.
        "MonaspiceNe Nerd Font"     ;; ( 7/10) High contrast and readable. Maybe the best Monaspice?
        "MonaspiceRn Nerd Font"     ;; ( 7/10) Nice, handwritten, high contrast and readable.
        "MonaspiceXe Nerd Font"     ;; ( 7/10) Serif, high contrast and readable.
        "AnonymicePro Nerd Font"    ;; ( 7/10) Interesting, similar to Inconsolata but spicier.
        "Binchotan_Sharp"           ;; ( 7/10) Small, futuristic, Asian vibes
        "DM Mono"                   ;; ( 6/10) Curvy and round, similar to 0xProto and Geist
        "Lekton Nerd Font"          ;; ( 6/10) Light, small, futuristic
        "IosevkaTermSlab Nerd Font" ;; ( 6/10) Interesting variant...
        "EnvyCodeR Nerd Font"       ;; ( 6/10) Not bad...
        "GoMono Nerd Font"          ;; ( 6/10) Not my thing, but a serif monospace font is pretty neat.
        "Monoid Nerd Font"          ;; ( 6/10) Thin like Iosevka.
        "ShureTechMono Nerd Font"   ;; ( 6/10) Futuristic and simple
        "CodeNewRoman Nerd Font"    ;; ( 6/10) On the smaller side. Very friendly and unprovocative. Monaco? Consolas?
        "MartianMono Nerd Font"     ;; ( 6/10) Thicc and curvy
        "DejaVu Sans Mono"          ;; ( 6/10) Boring, but solid.
        "Hack Nerd Font"            ;; ( 6/10) Boring
        "MesloLGS Nerd Font"        ;; ( 6/10) Boring
        "D2CodingLigature Nerd Font";; ( 6/10) Boring, like another Bitstream variant.
        "AverageMono"               ;; ( 5/10) Like Courier New. Typewriter font.
        "ComicShannsMono Nerd Font" ;; ( 5/10) Inferior to Comic Code
        "AurulentSansM Nerd Font"   ;; ( 5/10) Similar to Code New Roman
        "AtkynsonMono Nerd Font"    ;; ( 5/10) Similar to Source Code Pro and Inconsolata
        "IntoneMono Nerd Font"      ;; ( 5/10) Squished, too wide.
        "SpaceMono Nerd Font"       ;; ( 5/10) Very round
        "Azeret Mono"               ;; ( 5/10) Very high x-height
        "AdwaitaMono Nerd Font"     ;; ( 5/10) Large kerning but an OK font.
        "DaddyTimeMono Nerd Font"   ;; ( 5/10) Weird, but readable.
        "Monofur Nerd Font"         ;; ( 5/10) Futuristic and curvy. A little too much.
        "ProggyClean Nerd Font"     ;; ( 5/10) Too small and bad kerning
        "Source Code Pro"           ;; ( 4/10)
        "CaskaydiaMono Nerd Font"   ;; ( 4/10)
        ))

(setq my-default-font-size 12)
(setq my-font-state-file (concat user-emacs-directory "font-state.el"))

(defun my/font-installed-p (font-name)
  "Check if FONT-NAME is installed on the system."
  (member font-name (font-family-list)))

(defun my/get-first-available-font (font-list)
  "Return the first available font from FONT-LIST, or nil if none found."
  (seq-find #'my/font-installed-p font-list))

(defun my/save-font-state ()
  "Save font state to file."
  (with-temp-file my-font-state-file
    (prin1 `(setq my-current-font-family ,my-current-font-family)
           (current-buffer))))

(defun my/load-font-state ()
  "Load font state from file."
  (when (file-exists-p my-font-state-file)
    (load my-font-state-file)))

(defun my/set-font (font-name)
  "Set the font to FONT-NAME if available."
  (when (my/font-installed-p font-name)
    (set-face-attribute 'default nil
                        :family font-name
                        :height (* my-default-font-size 10))
    (setq my-current-font-family font-name)
    (my/save-font-state)
    (when (featurep 'centaur-tabs)
      (centaur-tabs-change-fonts font-name (* my-default-font-size 10)))
    (message "Font set to: %s" font-name)
    t))

(defun my/show-current-font ()
  "Display the current font family."
  (interactive)
  (message "Current font: %s" (face-attribute 'default :family)))

(defun my/cycle-font ()
  "Cycle through available fonts in my-font-list."
  (interactive)
  (let* ((available-fonts (seq-filter #'my/font-installed-p my-font-list))
         (current-index (or (cl-position my-current-font-family available-fonts :test #'string=) -1))
         (next-index (mod (1+ current-index) (length available-fonts)))
         (next-font (nth next-index available-fonts)))
    (if next-font
        (progn (my/set-font next-font))
      (message "No fonts from the list are installed!"))))

(defun my/set-font-interactive ()
  "Prompt for a font name and set it."
  (interactive)
  (let ((font-name (completing-read "Font: " (font-family-list))))
    (my/set-font font-name)))

(my/load-font-state)

(let ((font-to-use (if (and (boundp 'my-current-font-family)
                            (my/font-installed-p my-current-font-family))
                       my-current-font-family
                     "JetBrainsMono Nerd Font")))  ;; Fallback
  (my/set-font font-to-use))

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

;; Reload init safely
(defun my/reload-init ()
  "Reload init.el and re-enable evil-leader."
  (interactive)
  (load-file user-init-file)
  (when (fboundp 'evil-leader-mode)
    (evil-leader-mode 1)
    (evil-normalize-keymaps))
  (message "Init reloaded and evil-leader re-enabled"))

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
  "Kill buffer if its file has been deleted, disconnecting LSP first."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (not (file-exists-p buffer-file-name)))
        (when (and (fboundp 'lsp-disconnect) (bound-and-true-p lsp-mode))
          (ignore-errors (lsp-disconnect)))
        (set-buffer-modified-p nil)
        (kill-buffer buf)
        (message "Killed buffer for deleted file: %s" buffer-file-name)))))

;; Check on multiple triggers
(run-with-idle-timer 5 t #'my/kill-buffer-if-file-deleted)

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
;; TODO: it doesn't change when fonts get cycled, fix this
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
;; NOTE: evaluate this, and/or my/toggle-theme if new theme doesn't load properly.
(defun my/disable-all-bold ()
  "Remove bold from all faces."
  (mapc
   (lambda (face)
     (when (eq (face-attribute face :weight) 'bold)
       (set-face-attribute face nil :weight 'normal)))
   (face-list)))

;; Run after loading any theme
(advice-add 'load-theme :after
            (lambda (&rest _)
              (my/disable-all-bold)))

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-use-bold nil))
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
         (zig-mode . lsp)
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

(use-package zig-mode)

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
  (evil-leader/set-key "r r" 'my/reload-init)
  (evil-leader/set-key
    "x f" 'find-file
    "x b" 'switch-to-buffer
    "x k" 'kill-current-buffer)
  (evil-leader/set-key "]" 'centaur-tabs-forward)
  (evil-leader/set-key "[" 'centaur-tabs-backward)
  (evil-leader/set-key "q" 'centaur-tabs-backward)
  (evil-leader/set-key "e" 'centaur-tabs-forward)
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
    "f ?" 'my/show-current-font
    "f f" 'my/set-font-interactive
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
