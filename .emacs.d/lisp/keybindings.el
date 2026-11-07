;;; keybindings.el --- Evil and Evil-leader configuration -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-r") #'evil-redo))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil))  ;; Optional: avoid clutter
(setq evil-undo-system 'undo-tree)

(use-package general
  :ensure t
  :config
  (general-auto-unbind-keys) 
  (general-evil-setup)
  (general-define-key
    :states 'normal
    :prefix "SPC"

    "SPC" #'execute-extended-command 

    "l r" #'eglot-rename
    "l f" #'eglot-format-buffer
    "l a" #'eglot-code-actions
    "l o" #'eglot-code-action-organize-imports
    "l c" #'eglot-reconnect
    "l d" #'xref-find-definitions
    "l R" #'xref-find-references
    "l b" #'xref-pop-marker-stack

    "x f" #'find-file
    "x b" #'consult-buffer

    "g g" #'magit-status
    "g d" #'magit-dispatch

    "s s" #'consult-line
    "s r" #'consult-ripgrep
))

;; Keybinding help
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.05))

(provide 'keybindings)
