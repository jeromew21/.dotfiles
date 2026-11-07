;;; editing.el --- Editing configuration -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t)
(setq initial-scratch-message "hello again...")

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message ()
  (message nil))

(electric-pair-mode 1)

(global-auto-revert-mode 1)

(recentf-mode 1)

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

;; TODO: do we even care about embark?
;; TODO: integrate into the leader keybindings
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

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package evil-nerd-commenter)

(require 'project)

(provide 'editing)
