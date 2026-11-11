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

(electric-pair-mode 1)

(global-auto-revert-mode 1)

(recentf-mode 1)

(setq scroll-conservatively 101)
(setq scroll-margin 0)

(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)

(require 'project)

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
 (setq evil-want-keybinding nil)
 (setq evil-undo-system 'undo-fu)
 (setq evil-want-integration t)
 :config
 (evil-mode 1)
 (define-key evil-normal-state-map (kbd "C-r") #'evil-redo))

(use-package evil-collection :after evil :config (evil-collection-init))

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

(use-package vertico :init (vertico-mode) :custom (vertico-cycle t))

(use-package
 marginalia
 ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
 ;; available in the *Completions* buffer, add it to the
 ;; `completion-list-mode-map'.
 :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
 :init (marginalia-mode))

(use-package consult :ensure t)
(setq consult-preview-key nil)

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

(use-package magit :ensure t)

(use-package treesit-auto :config (global-treesit-auto-mode))

(use-package avy :ensure t)

(use-package expand-region :ensure t)

(use-package multiple-cursors :ensure t)

(use-package emojify :hook (after-init . global-emojify-mode))

(use-package evil-nerd-commenter)

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

(use-package
 copilot
 :ensure t
 :hook (prog-mode . copilot-mode)
 :bind
 (:map
  copilot-completion-map
  ("C-<tab>" . copilot-accept-completion)
  ("C-n" . copilot-next-completion)
  ("C-p" . copilot-previous-completion)))

;; TODO: if auto indentations start being off, fix this.
(setq copilot-indent-offset-warning-disable t)

(use-package nov :ensure t :mode ("\\.epub\\'" . nov-mode) :config (setq nov-text-width 80))
(add-hook 'nov-mode-hook #'visual-line-mode)
(add-hook 'nov-mode-hook (lambda () (display-line-numbers-mode -1)))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'shr-text :family "Times New Roman"
                                     :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(use-package
 pdf-tools
 :ensure t
 :demand t
 :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
 :config (pdf-tools-install)
 (with-eval-after-load 'evil-collection
   (evil-collection-pdf-setup)))

(provide 'editing)
