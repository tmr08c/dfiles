;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Fira Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-theme 'doom-molokai
      doom-big-font (font-spec :family "Fira Mono" :size 19))

(def-package! cheatsheet
  :commands (cheatsheet-add cheatsheet-add-group cheatsheet-get cheatsheet-show))

(after! company
  (setq company-idle-delay 0.6
        company-minimum-prefix-length 2))

(after! projectile
  (setq projectile-globally-ignored-file-suffixes (append (list ".elc"))
        projectile-globally-ignored-directories (append (list "node_modules"))
        projectile-track-known-projects-automatically nil
        counsel-projectile-sort-projects t
        projectile-ignored-projects nil))
