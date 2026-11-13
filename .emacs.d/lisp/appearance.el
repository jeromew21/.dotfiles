;;; appearance.el --- UI configuration -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-title-format "Emacs")
(setq ns-use-proxy-icon nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(setq display-line-numbers-type t
      display-line-numbers-width-start t)

;; Personally, I don't believe in relative line numbers
(global-display-line-numbers-mode 1)

;; TODO: this whitespace needs to only show in programming modes, not text modes. I need to figure out how to do that.
(setq-default show-trailing-whitespace t)
(setq-default fill-column 100)

;; The fill-column-indicator is a nice visual indicator of the fill column, but it can be distracting. I will leave it off for now.
;; (global-display-fill-column-indicator-mode 1)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(use-package doom-modeline
  :ensure t
  :after doom-themes
  :hook (window-setup . doom-modeline-mode)
  :custom
  ;; Your existing layout & minimal UI settings
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-file-name-style 'auto)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-enable-word-count nil)

  ;; Performance & background-check trims
  (doom-modeline-project-detection 'project)
  (doom-modeline-check-simple-format t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-irc nil))

;; Lotta times this doesn't work, but at least it doesn't show literal ANSI color codes...
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Preserve compilation output colors
(setq compilation-scroll-output 'first-error)

(provide 'appearance)
