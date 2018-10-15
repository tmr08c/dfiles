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
    (setq tab-width 2
          indent-tabs-mode 1)
    (flycheck-gometalinter-setup)
    (flycheck-mode 1))
  (add-hook 'go-mode-hook #'my-go-mode-hook-fn))

(after! company
  (setq company-idle-delay 0.6
        company-minimum-prefix-length 2))

(after! counsel
  (setq counsel-rg-base-command "rg -zS -M 150 --no-heading --line-number --color never %s ."))

(after! ivy
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(after! neotree
  (setq neo-theme 'icons)

  ;; Reset Neotree Keymap so +defaults are not merged in with our customizations.
  (setq neotree-mode-map (make-sparse-keymap))
  (map! :map neotree-mode-map
        :n "RET" #'neotree-enter
        :n "TAB" #'neotree-stretch-toggle
        :n "q" #'neotree-hide
        :n "|" #'neotree-enter-vertical-split
        :n "-" #'neotree-enter-horizontal-split
        :n "'" #'neotree-quick-look
        :n "c" #'neotree-create-node
        :n "C" #'neotree-copy-node
        :n "d" #'neotree-delete-node
        :n "gr" #'neotree-refresh

        :n "h" #'+neotree/collapse-or-up
        :n "H" #'neotree-select-previous-sibling-node
        :n "j" #'neotree-next-line
        :n "J" #'neotree-select-down-node
        :n "k" #'neotree-previous-line
        :n "K" #'neotree-select-up-node
        :n "L" #'neotree-select-next-sibling-node

        :n "l" #'+neotree/expand-or-open
        :n "q" #'neotree-hide
        :n "o" #'neotree-enter
        :n "r" #'neotree-rename-node
        :n "R" #'neotree-change-root
        :n "I" #'neotree-hidden-file-toggle))

(after! projectile
  (setq projectile-globally-ignored-file-suffixes (append (list ".elc"))
        projectile-globally-ignored-directories (append (list "node_modules"))
        projectile-track-known-projects-automatically nil
        counsel-projectile-sort-projects t
        projectile-ignored-projects nil))

(after! swiper
  (map!
   :n "C-s" 'swiper))

(after! which-key
  (setq which-key-idle-delay 0.8))

