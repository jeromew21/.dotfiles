;;; appearance.el --- Theme and look configuration -*- lexical-binding: t; -*-

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(setq display-line-numbers-type t
      display-line-numbers-width-start t)

(global-display-line-numbers-mode 1)

(setq-default show-trailing-whitespace t)
(setq-default fill-column 100)

(global-display-fill-column-indicator-mode 1)

;; Disable UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq frame-title-format
      (format "Emacs %d.%d - %%b" emacs-major-version emacs-minor-version))

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

(load-theme 'modus-operandi-tinted t)

(provide 'appearance)
