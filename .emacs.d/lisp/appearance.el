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
      (format "Emacs %d.%d" emacs-major-version emacs-minor-version))

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

(use-package ef-themes
  :ensure t
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.  The manual of
  ;; the Ef themes has a section that explains all the possibilities:
  ;;
  ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
  ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
  (ef-themes-take-over-modus-themes-mode 1)
  ;; :bind
  ;; (("<f5>" . modus-themes-rotate)
  ;;  ("C-<f5>" . modus-themes-select)
  ;;  ("M-<f5>" . modus-themes-load-random))
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-arbutus))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Preserve compilation output colors
(setq compilation-scroll-output 'first-error)

;; Allow ANSI colors in compilation buffers
(add-hook 'compilation-filter-hook
          'ansi-color-compilation-filter)

(provide 'appearance)
