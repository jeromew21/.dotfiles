;;; keybindings.el --- Evil and Evil-leader configuration -*- lexical-binding: t; -*-

(use-package
 which-key
 :ensure t
 :config
 (which-key-mode)
 (setq which-key-idle-delay 0.2)
 (setq which-key-idle-secondary-delay 0.05))

(use-package
 general
 :ensure t
 :config (general-auto-unbind-keys) (general-evil-setup)

 (general-define-key
  :states '(normal motion)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC"

  "SPC"
  #'execute-extended-command

  "/"
  #'evilnc-comment-or-uncomment-lines

  "l"
  '(:ignore t :which-key "lsp")
  "l r"
  #'eglot-rename
  "l f"
  #'eglot-format-buffer
  "l a"
  #'eglot-code-actions
  "l o"
  #'eglot-code-action-organize-imports
  "l c"
  #'eglot-reconnect
  "l d"
  #'xref-find-definitions
  "l R"
  #'xref-find-references
  "l b"
  #'xref-pop-marker-stack

  "b"
  '(:ignore t :which-key "buffers")
  "b b"
  #'consult-buffer
  "b n"
  #'next-buffer
  "b p"
  #'previous-buffer
  "b d"
  #'kill-current-buffer
  "b k"
  #'kill-buffer
  "b s"
  #'save-buffer
  "b r"
  #'revert-buffer

  "f"
  '(:ignore t :which-key "files")
  "f f"
  #'find-file
  "f s"
  #'save-buffer
  "f r"
  #'recentf-open-files
  "f d"
  #'dired
  "f D"
  #'dired-jump

  "F"
  '(:ignore t :which-key "fonts")
  "F +"
  'my/increase-font-size
  "F -"
  'my/decrease-font-size
  "F ="
  'my/reset-font-size
  "F ?"
  'my/show-current-font
  "F f"
  'my/set-font-interactive
  "F c"
  'my/cycle-font

  "g"
  '(:ignore t :which-key "git")
  "g s"
  #'magit-status
  "g d"
  #'magit-dispatch

  "s"
  '(:ignore t :which-key "search")
  "s s"
  #'consult-line
  "s r"
  #'consult-ripgrep
  "s g"
  #'consult-goto-line
  "s i"
  #'consult-imenu

  "p"
  '(:keymap project-prefix-map :package project :which-key "project")
  "p c"
  #'project-compile
  "p r"
  #'project-recompile
  "p !"
  #'project-shell-command
  "p &"
  #'project-async-shell-command
  "p s"
  #'project-shell

  "y"
  '(:keymap ctl-x-map :which-key "C-x commands")

  "d"
  '(:ignore t :which-key "diagnostics")
  "d d"
  #'flymake-show-buffer-diagnostics
  "d n"
  #'flymake-goto-next-error
  "d p"
  #'flymake-goto-prev-error
  "d l"
  #'flymake-show-project-diagnostics

  "e"
  '(:ignore t :which-key "evaluation")
  "e c"
  #'project-compile
  "e r"
  #'project-recompile
  "e k"
  #'kill-compilation
  "e b"
  #'eval-buffer
  "e d"
  #'eval-defun

  "w"
  '(:ignore t :which-key "windows")
  "w h"
  #'windmove-left
  "w j"
  #'windmove-down
  "w k"
  #'windmove-up
  "w l"
  #'windmove-right
  "w v"
  #'split-window-right
  "w s"
  #'split-window-below
  "w d"
  #'delete-window
  "w o"
  #'delete-other-windows
  "w u"
  #'winner-undo
  "w r"
  #'winner-redo

  "h"
  '(:ignore t :which-key "help")
  "h f"
  #'describe-function
  "h v"
  #'describe-variable
  "h k"
  #'describe-key
  "h m"
  #'describe-mode
  "h p"
  #'describe-package
  "h c"
  #'describe-char))

(keymap-global-set "<f5>" #'project-recompile)

(which-key-add-key-based-replacements "SPC SPC" "M-x" "SPC /" "comment/uncomment")

(provide 'keybindings)
