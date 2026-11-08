;;; fonts.el --- Font configuration -*- lexical-binding: t; -*-

;; Font setup
(setq
 my-font-list
 '("JetBrains Mono" ;; (10/10) Current favorite, all round good.
   "Rec Mono Casual" ;; ( 9/10) Goofy version of Rec Mono. This might be the one.
   "Rec Mono Linear" ;; ( 9/10) Nice font, like Fira but better.
   "Fira Code" ;; ( 9/10) Iconic, sharp, wide, to pointy. Very nice on most screens. The Primagen font.
   "Comic Code" ;; ( 9/10) Good font, but the line height seems wrong on this one on Emacs.
   "FantasqueSansM Nerd Font" ;; ( 9/10) Small, but nice goofy font.
   "0xProto Nerd Font" ;; ( 9/10) Dyslexic font vibes. Readable, very high x-height, like a better Cascadia Code.
   "CommitMono Nerd Font" ;; ( 8/10) Very round and with serifs, it's interesting. Similar to Inconsolata.
   "Maple Mono NL" ;; ( 8/10) Cutesy, Japanese vibes.
   "JuliaMono" ;; ( 8/10) Pretty good. Like 0xProto and Fira but less going on.
   "Sudo Var" ;; ( 8/10) Like Iosevka but prettier
   "M+1Code Nerd Font" ;; (8/10) Slightly villainous vibes. Similar to Sudo Var but pointier. Looks better big.
   "IBM Plex Mono" ;; (9/10) Really solid, no notes
   "SF Mono" ;; ( 7/10) Solid, similar to JetBrains mono, worse numbers.
   "Inconsolata Nerd Font" ;; ( 9/10) Another solid all-rounder. Weirdly, it's smaller than the others.
   "VictorMono Nerd Font" ;; ( 8/10) Futuristic and thin, nice.
   "DepartureMono Nerd Font" ;; ( 8/10) Nice retro font, very readable but not ugly. Maybe try disabling AA?
   "Noto Sans Mono" ;; ( 7/10) Boring, inoffensive. New version of Droid Sans Mono.
   "ProggyClean Nerd Font" ;; ( 5/10) Too small and bad kerning
   "Terminus" ;; ( 7/10) Nice retro font.
   "DM Mono" ;; ( 6/10) Curvy and round, similar to 0xProto and Geist
   "ProFont IIx Nerd Font" ;; ( 7/10) Simple, retro-futuristic
   "Iosevka Nerd Font" ;; ( 7/10) Efficient, skinny, iconic. The Tsoding font.
   "Cousine Nerd Font" ;; ( 7/10) Highly familiar looking font. Identical as Liberation Mono.
   "Hurmit Nerd Font" ;; ( 7/10) Unique, Reads okay
   "BigBlueTerm437 Nerd Font" ;; ( 7/10) I think this is the GRUB font. It's iconic
   "GeistMono Nerd Font" ;; ( 7/10) Very round and slightly weird, but fine.
   "Overpass Mono" ;; ( 7/10) Nerd font version is broken
   "RobotoMono Nerd Font" ;; ( 7/10) Readable, similar to JetBrains Mono. A bit boring ff.
   "Monaco" ;; ( 7/10) Iconic, boring. Lucida Console?
   "MonaspiceAr Nerd Font" ;; ( 7/10) High contrast and readable.
   "MonaspiceKr Nerd Font" ;; ( 7/10) Sharp variant, high contrast and readable.
   "MonaspiceNe Nerd Font" ;; ( 7/10) High contrast and readable. Maybe the best Monaspice?
   "MonaspiceRn Nerd Font" ;; ( 7/10) Nice, handwritten, high contrast and readable.
   "MonaspiceXe Nerd Font" ;; ( 7/10) Serif, high contrast and readable.
   "AnonymicePro Nerd Font" ;; ( 7/10) Interesting, similar to Inconsolata but spicier.
   "Binchotan_Sharp" ;; ( 7/10) Small, futuristic, Asian vibes
   "Lekton Nerd Font" ;; ( 6/10) Light, small, futuristic
   "IosevkaTermSlab Nerd Font" ;; ( 6/10) Interesting variant...
   "EnvyCodeR Nerd Font" ;; ( 6/10) Not bad...
   "GoMono Nerd Font" ;; ( 6/10) Not my thing, but a serif monospace font is pretty neat.
   "Monoid Nerd Font" ;; ( 6/10) Thin like Iosevka.
   "ShureTechMono Nerd Font" ;; ( 6/10) Futuristic and simple
   "CodeNewRoman Nerd Font" ;; ( 6/10) On the smaller side. Very friendly and unprovocative. Monaco? Consolas?
   "MartianMono Nerd Font" ;; ( 6/10) Thicc and curvy
   "DejaVu Sans Mono" ;; ( 6/10) Boring, but solid.
   "Hack Nerd Font" ;; ( 6/10) Boring
   "MesloLGS Nerd Font" ;; ( 6/10) Boring
   "D2CodingLigature Nerd Font" ;; ( 6/10) Boring, like another Bitstream variant.
   "AverageMono" ;; ( 5/10) Like Courier New. Typewriter font.
   "ComicShannsMono Nerd Font" ;; ( 5/10) Inferior to Comic Code
   "AurulentSansM Nerd Font" ;; ( 5/10) Similar to Code New Roman
   "AtkynsonMono Nerd Font" ;; ( 5/10) Similar to Source Code Pro and Inconsolata
   "IntoneMono Nerd Font" ;; ( 5/10) Squished, too wide.
   "SpaceMono Nerd Font" ;; ( 5/10) Very round
   "Azeret Mono" ;; ( 5/10) Very high x-height
   "AdwaitaMono Nerd Font" ;; ( 5/10) Large kerning but an OK font.
   "DaddyTimeMono Nerd Font" ;; ( 5/10) Weird, but readable.
   "Monofur Nerd Font" ;; ( 5/10) Futuristic and curvy. A little too much.
   "Source Code Pro" ;; ( 4/10)
   "CaskaydiaMono Nerd Font" ;; ( 4/10)
   ))

(setq my-font-size 12)
(setq my-font-state-file (concat user-emacs-directory "font-state.el"))

(defun my/font-installed-p (font-name)
  "Check if FONT-NAME is installed on the system."
  (member font-name (font-family-list)))

(defun my/get-first-available-font (font-list)
  "Return the first available font from FONT-LIST, or nil if none found."
  (seq-find #'my/font-installed-p font-list))

(defun my/save-font-state ()
  "Save font state to file."
  (with-temp-file my-font-state-file
    (insert ";;; -*- lexical-binding: t; -*-\n\n")
    (insert ";; This file is auto-generated by fonts.el. Do not edit manually.\n\n")
    (prin1 `(setq
             my-current-font-family ,my-current-font-family
             my-font-size ,my-font-size)
           (current-buffer))))

(defun my/load-font-state ()
  "Load font state from file."
  (when (file-exists-p my-font-state-file)
    (load my-font-state-file)))

(defun my/set-font (font-name)
  "Set the font to FONT-NAME if available."
  (when (my/font-installed-p font-name)
    (set-face-attribute 'default nil :family font-name :height (* my-font-size 10))
    (setq my-current-font-family font-name)
    (my/save-font-state)
    (when (featurep 'centaur-tabs)
      (centaur-tabs-change-fonts font-name (* my-font-size 10)))
    (message "Font set to: %s" font-name)
    t))

(defun my/show-current-font ()
  "Display the current font family."
  (interactive)
  (message "Current font: %s" (face-attribute 'default :family)))

(defun my/cycle-font ()
  "Cycle through available fonts in my-font-list."
  (interactive)
  (let* ((available-fonts (seq-filter #'my/font-installed-p my-font-list))
         (current-index
          (or (cl-position my-current-font-family available-fonts :test #'string=) -1))
         (next-index (mod (1+ current-index) (length available-fonts)))
         (next-font (nth next-index available-fonts)))
    (if next-font
        (progn
          (my/set-font next-font))
      (message "No fonts from the list are installed!"))))

(defun my/set-font-interactive ()
  "Prompt for a font name and set it."
  (interactive)
  (let ((font-name (completing-read "Font: " (font-family-list))))
    (my/set-font font-name)))

(my/load-font-state)

(let ((font-to-use
       (if (and (boundp 'my-current-font-family) (my/font-installed-p my-current-font-family))
           my-current-font-family
         "JetBrainsMono Nerd Font"))) ;; Fallback
  (my/set-font font-to-use))

(defun my/increase-font-size ()
  "Increase font size by 1."
  (interactive)
  (setq my-font-size (+ my-font-size 1))
  (set-face-attribute 'default nil :height (* my-font-size 10))
  ;; Update centaur-tabs if loaded
  (when (featurep 'centaur-tabs)
    (centaur-tabs-change-fonts (face-attribute 'default :family) (face-attribute 'default :height)))
  (message "Font size: %d" my-font-size))

(defun my/decrease-font-size ()
  "Decrease font size by 1."
  (interactive)
  (setq my-font-size (- my-font-size 1))
  (set-face-attribute 'default nil :height (* my-font-size 10))
  ;; Update centaur-tabs if loaded
  (when (featurep 'centaur-tabs)
    (centaur-tabs-change-fonts (face-attribute 'default :family) (face-attribute 'default :height)))
  (message "Font size: %d" my-font-size))

(defun my/reset-font-size ()
  "Reset font size to default (12)."
  (interactive)
  (setq my-font-size 12)
  (set-face-attribute 'default nil :height (* my-font-size 10))
  (when (featurep 'centaur-tabs)
    (centaur-tabs-change-fonts (face-attribute 'default :family) (face-attribute 'default :height)))
  (message "Font size reset to: %d" my-font-size))

(provide 'fonts)
