;;; appearance.el --- UI configuration -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(setq display-line-numbers-type t
      display-line-numbers-width-start t)

(global-display-line-numbers-mode 1)

(setq-default show-trailing-whitespace t)
(setq-default fill-column 100)

;; (global-display-fill-column-indicator-mode 1)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-enable-word-count nil)
  :config
  (doom-modeline-mode 1))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Preserve compilation output colors
(setq compilation-scroll-output 'first-error)

(provide 'appearance)
