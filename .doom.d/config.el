;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

(setq doom-font (if IS-MAC
                  ; Font size is funny on macOS
                  (font-spec :family "Fira Mono" :size 13)
                  (font-spec :family "Fira Mono" :size 15)))

(setq doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-theme 'doom-molokai
      doom-big-font (font-spec :family "Fira Mono" :size 19))

(let ((emoji-font-face
       (if IS-MAC "Apple Color Emoji" "Twitter Color Emoji")))
  (setq doom-unicode-font
        (font-spec :name emoji-font-face :size 14)))

(when IS-LINUX
  (map!
   ;; Use Super-S to save like I am on macOS
   :n "s-s" (λ! (call-interactively (key-binding "\C-x\C-s")))))

(def-package! aggressive-indent
  :hook ((emacs-lisp-mode css-mode lisp-mode) . aggressive-indent-mode))

(after! company
  (setq company-idle-delay 0.6
        company-minimum-prefix-length 2))
(after! ivy
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(after! projectile
  (setq projectile-globally-ignored-file-suffixes (append (list ".elc"))
        projectile-globally-ignored-directories (append (list "node_modules"))
        projectile-track-known-projects-automatically nil
        counsel-projectile-sort-projects t
        projectile-ignored-projects nil))

(after! which-key
  (setq which-key-idle-delay 0.8))

