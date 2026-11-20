;;; editing.el --- Editing configuration -*- lexical-binding: t; -*-

(prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-language-environment 'utf-8)

(setq
 make-backup-files nil
 auto-save-default nil)

(use-package
 desktop
 :ensure nil
 :custom
 (desktop-save t)
 (desktop-load-locked-desktop t)
 (desktop-auto-save-timeout 300)
 :config (desktop-save-mode 1))

(setq desktop-restore-eager 1)
(setq desktop-modes-not-to-save
      '(pdf-view-mode
        nov-mode tags-table-mode dired-mode Info-mode info-lookup-mode fundamental-mode))

(electric-pair-mode 1)

(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info nil)

(use-package
 recentf
 :ensure nil
 :init
 ;; Wait until Emacs is idle for 1s after startup before reading the file
 (run-with-idle-timer 1 nil #'recentf-mode)
 :custom
 (recentf-auto-cleanup 'never)
 (recentf-max-saved-items 200)
 (recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "eln-cache/" "recentf$" "bookmarks$")))

(setq scroll-conservatively 101)
(setq scroll-margin 0)

(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)

(require 'project)

;; Automatically kill buffers untouched for 2 days
(use-package
 midnight
 :ensure nil
 :custom (clean-buffer-list-delay-general 2)
 :config (midnight-mode 1))

(use-package
 super-save
 :ensure t
 :config
 (setq
  super-save-auto-save-when-idle nil
  super-save-all-buffers t
  super-save-silent t)
 (super-save-mode 1))

(use-package restart-emacs :ensure t :commands restart-emacs)

(use-package undo-fu :ensure t)

(use-package
 evil
 :ensure t
 :init
 ;; Flags read WHILE evil loads (must stay in :init)
 (setq
  evil-want-integration t
  evil-want-keybinding nil
  evil-disable-insert-state-bindings t ; Skip ~100 Emacs chords in Insert mode
  evil-want-Y-yank-to-eol t) ; Make Y yank to end of line (like D and C)
 :custom
 ;; Options with :set handlers (evaluated properly via :custom)
 (evil-undo-system 'undo-fu)
 (evil-search-module 'evil-search) ; Proper Vim /search and :noh highlighting
 :config (evil-mode 1))

(use-package evil-collection :ensure t :after evil :config (evil-collection-init))

(use-package
 general
 :after evil-collection
 :ensure t
 :config (general-auto-unbind-keys) (general-evil-setup)

 (general-create-definer
  my-leader
  :states '(normal motion visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC"))

(use-package
 corfu
 :ensure t
 :init (global-corfu-mode)
 :custom
 (corfu-auto t)
 (corfu-auto-delay 0.2)
 (corfu-auto-prefix 2)
 (corfu-cycle t)
 (corfu-preselect 'prompt))

(use-package
 cape
 :ensure t
 :init
 (add-to-list 'completion-at-point-functions #'cape-dabbrev)
 (add-to-list 'completion-at-point-functions #'cape-file))

(use-package
 orderless
 :ensure t
 :custom
 (completion-styles '(orderless basic))
 (completion-category-defaults nil)
 (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico :ensure t :init (vertico-mode) :custom (vertico-cycle t))

(use-package
 marginalia
 :ensure t
 ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
 ;; available in the *Completions* buffer, add it to the
 ;; `completion-list-mode-map'.
 :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
 :init (marginalia-mode))

(use-package consult :ensure t)
(setq consult-preview-key nil)
(with-eval-after-load 'consult
  (add-to-list 'consult-buffer-filter "\\`\\*.*\\*\\'"))

;; Search project for symbols using eglot and consult
(use-package consult-eglot :ensure t :after (eglot consult))

;; TODO: do we even care about embark?
(use-package
 embark
 :ensure t
 :bind (("C-." . embark-act) ("C-;" . embark-dwim) ("C-h B" . embark-bindings)))

(use-package
 embark-consult
 :ensure t
 :after (embark consult)
 :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit :ensure t :commands (magit-status magit-dispatch magit-file-dispatch))

(use-package treesit-auto :ensure t :config (global-treesit-auto-mode))

(use-package avy :ensure t)

(use-package expand-region :ensure t :defer t)

(use-package multiple-cursors :ensure t :defer t)

(use-package evil-nerd-commenter :ensure t :defer t)

(use-package
 vterm
 :commands vterm
 :config
 ;; Maximize the scrollback buffer size
 (setq vterm-max-scrollback 10000))

(use-package
 imenu-list
 :ensure t
 :commands (imenu-list-smart-toggle)
 :init
 (setq
  imenu-list-focus-after-activation t
  imenu-list-auto-resize nil))

(use-package consult :ensure t)

;; github copilot
;; TODO: try an alternative w/ Llama local model
(defvar my/copilot-warned nil)

(defun my/copilot-ready-p ()
  "Return non-nil if the Copilot language server or Node.js is available."
  (or (and (fboundp 'copilot--server-executable)
           (ignore-errors
             (copilot--server-executable)))
      (executable-find "copilot-language-server") (executable-find "node")))

(defun my/enable-copilot-if-ready ()
  "Enable `copilot-mode' only if `copilot' and its server/Node.js exist."
  (if (and (require 'copilot nil t) (my/copilot-ready-p))
      (copilot-mode 1)
    (unless my/copilot-warned
      (setq my/copilot-warned t)
      (message
       "[config] copilot disabled: run M-x copilot-install-server (or install Node.js 22+ & npm)."))))

(use-package
 copilot
 :ensure t
 :defer t
 :init
 ;; TODO: if auto indentations start being off, fix this.
 ;; They kind of are off right now
 (setq copilot-indent-offset-warning-disable t)
 (add-hook 'prog-mode-hook #'my/enable-copilot-if-ready)
 :bind
 (:map
  copilot-completion-map
  ("C-<tab>" . copilot-accept-completion)
  ("C-n" . copilot-next-completion)
  ("C-p" . copilot-previous-completion))
 :catch
 (lambda (_keyword err)
   (message "[config] copilot package failed to load: %s" (error-message-string err))))

;; epub reading
(defun my-nov-font-setup ()
  (face-remap-add-relative 'shr-text :family "Charter" :height 1.0))

(if (and (executable-find "unzip") (fboundp 'libxml-parse-html-region))
    (use-package
     nov
     :ensure t
     :mode ("\\.epub\\'" . nov-mode)
     :hook
     ((nov-mode . visual-line-mode)
      (nov-mode . my-nov-font-setup)
      (nov-mode . (lambda () (display-line-numbers-mode -1))))
     :config (setq nov-text-width 80)
     :catch
     (lambda (_keyword err)
       (message "[config] nov package failed to load: %s" (error-message-string err))))
  (message
   "[config] nov disabled: install `unzip' (and ensure Emacs has libxml2 support) to read EPUB files."))

;; pdf reading
(defun my/pdf-tools-deps-ready-p ()
  "Return non-nil if `epdfinfo' exists or C/C++ build tools are available."
  (or (executable-find "epdfinfo")
      (and (executable-find "make")
           (executable-find "pkg-config")
           (or (executable-find "gcc") (executable-find "clang")))))

(if (my/pdf-tools-deps-ready-p)
    (use-package
     pdf-tools
     :ensure t
     :magic ("%PDF" . pdf-view-mode)
     :hook
     ((pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
      (pdf-view-mode
       .
       (lambda ()
         (when (fboundp 'pdf-view-roll-minor-mode)
           (pdf-view-roll-minor-mode 1)))))
     :custom
     ;; Visual gap between pages in continuous scroll mode
     (pdf-roll-vertical-margin 8) (pdf-roll-margin-color "gray30")
     :config
     (condition-case err
         (pdf-tools-install :no-query nil t)
       (error
        (message
         "[config] pdf-tools server build failed (%s). Install `libpoppler-glib-dev', `zlib1g-dev', `autoconf', and `automake'."
         (error-message-string err))))
     :catch
     (lambda (_keyword err)
       (message "[config] pdf-tools package failed to load: %s" (error-message-string err))))
  (message
   "[config] pdf-tools disabled: install `epdfinfo' or build tools (`make', `pkg-config', `gcc'/`clang', `libpoppler-glib-dev', `zlib1g-dev')."))

;; Optional: enables smooth pixel-by-pixel trackpad/mouse-wheel scrolling
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(provide 'editing)
