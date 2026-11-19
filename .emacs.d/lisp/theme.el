;;; theme.el --- Theme configuration -*- lexical-binding: t; -*-

(defun my/load-doom-theme (theme)
  "Disable active themes, load Doom THEME, and sync custom faces."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  ;; Keep keycap badges synced with the active Solarized palette
  (set-face-attribute 'help-key-binding nil
                      :inherit 'fixed-pitch
                      :foreground (doom-color 'blue)
                      :background (doom-color 'bg-alt)
                      :box `(:line-width (-1 . -1) :color ,(doom-color 'base4))))

(defun my/toggle-solarized-theme ()
  "Toggle between `doom-solarized-light' and `doom-solarized-dark'."
  (interactive)
  (let ((next
         (if (memq 'doom-solarized-light custom-enabled-themes)
             'doom-solarized-dark
           'doom-solarized-light)))
    (my/load-doom-theme next)
    (message "Theme: %s" next)))

(use-package
 doom-themes
 :ensure t
 :custom
 (doom-themes-enable-bold nil)
 (doom-themes-enable-italic nil)
 :config
 (my/load-doom-theme 'doom-solarized-light)
 (set-face-attribute 'font-lock-comment-face nil :slant 'italic))

(provide 'theme)
