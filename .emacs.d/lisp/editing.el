;;; editing.el --- Editing configuration -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Electric pairing
(electric-pair-mode 1)

;; Auto-revert
(global-auto-revert-mode 1)

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions
               #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions
               #'cape-file))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; TODO: move these to leader keys.
(use-package consult
  :ensure t
  :bind
  (;; Search

   ;; Other useful commands
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit
  :ensure t
)

(provide 'editing)
