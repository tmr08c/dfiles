;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

(setq doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-theme 'doom-molokai
      doom-big-font (font-spec :family "Fira Mono" :size 19))

(let ((emoji-font-face
       (if IS-MAC "Apple Color Emoji" "Twitter Color Emoji"))
      (font-size
       (if IS-MAC 13 15)))
  (setq doom-unicode-font (font-spec :name emoji-font-face :size font-size)
        doom-font (font-spec :family "Fira Mono" :size font-size)))

(when IS-LINUX
  (map!
   ;; Use Super-S to save like I am on macOS
   :n "s-s" (λ! (call-interactively (key-binding "\C-x\C-s")))))

(def-package! aggressive-indent
  :hook ((emacs-lisp-mode css-mode lisp-mode) . aggressive-indent-mode))

(def-package! flycheck-gometalinter
  :commands flycheck-gometalinter-setup
  :custom
  (flycheck-gometalinter-vendor t) ; skip linting for vendor dirs
  (flycheck-gometalinter-test t)   ; use in test files
  (flycheck-gometalinter-fast t)   ; only use fast linters
  ;; explicitly disable 'gotype' & 'govet' linters (also currently broken Nix overlays)
  (flycheck-gometalinter-disable-linters
   '("gosec" "gotype" "vet" "vetshadow" "megacheck" "interfacer" "ineffassign")))

(after! doom-themes
  (remove-hook! 'doom-load-theme-hook 'doom-themes-neotree-config))

(after! go-mode
  (add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on save

  (defun my-go-mode-hook-fn ()
    ;; (go-eldoc-setup)
    ;; (setq-local company-backends '(company-go))
    (setq tab-width 2
          indent-tabs-mode 1)
    (flycheck-gometalinter-setup)
    (flycheck-mode 1))
  (add-hook 'go-mode-hook #'my-go-mode-hook-fn))

(after! company
  (setq company-idle-delay 0.6
        company-minimum-prefix-length 2))
(after! ivy
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(after! neotree
  (setq neo-theme 'icons))

(after! projectile
  (setq projectile-globally-ignored-file-suffixes (append (list ".elc"))
        projectile-globally-ignored-directories (append (list "node_modules"))
        projectile-track-known-projects-automatically nil
        counsel-projectile-sort-projects t
        projectile-ignored-projects nil))

(after! which-key
  (setq which-key-idle-delay 0.8))

