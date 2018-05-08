;;; init --- Justin Smestad's Emacs init file
;;; Commentary:

;;; Code:
(customize-set-variable 'gc-cons-threshold (* 10 1024 1024))

;; Default to UTF-8 early as this file uses Unicode symbols.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Get package repos configured
(require 'package)
(customize-set-variable
 'package-archives
 '(("melpa" . "https://melpa.org/packages/")
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("org" . "https://orgmode.org/elpa/")))
(unless package--initialized
  (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(customize-set-variable 'use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

(use-package delight)
(use-package dash)

;; Ensure system has required packages and install if missing
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))
(use-package use-package-ensure-system-package)
(use-package system-packages
  :requires use-package-ensure-system-package)

;; Save data files consistently:
;; - `save-place-file'
;; - `undo-tree-history-directory-alist'
;; - `backup-directory-alist'
;; - etc.
(use-package no-littering)

(customize-set-variable
 'custom-file (no-littering-expand-var-file-name "custom.el"))

(customize-set-variable 'user-full-name "Justin Smestad")
(customize-set-variable 'user-mail-address "justin.smestad@gmail.com")


;;; Key Bindings
(use-package general
  ;; :custom
  ;; (general-default-prefix "SPC")
  ;; (general-default-non-normal-prefix "C-SPC")
  :config
  (general-create-definer space-leader-def
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-evil-setup)
  (space-leader-def
    :states '(normal visual insert emacs)

    "TAB" '(switch-to-other-buffer :which-key "prev buffer")

    "f"   '(:ignore t :which-key "Files")
    "f t" '(neotree-toggle :which-key "toggle file tree")

    "p"   '(:ignore t :which-key "Projects")
    "p t" '(neotree-find-project-root :which-key "project tree")
    "p p" '(projectile-switch-project :which-key "open project")

    "q"   '(:ignore t :which-key "Quit")
    "q r" '(restart-emacs :which-key "restart")

    "w"   '(:ignore t :which-key "Windows")
    "w /" '(split-window-right :which-key "split vertical")
    "w -" '(split-window-below :which-key "split horizontal")

    "SPC" '(counsel-M-x :which-key "M-x")))

;; Platform
;;
(use-package linux
  :disabled
  :load-path "vendor/"
  :ensure nil
  :if (eq system-type 'gnu/linux))

(use-package osx
  :load-path "vendor/"
  :if (eq system-type 'darwin))

(use-package windows
  :disabled
  :load-path "vendor/"
  :ensure nil
  :if (eq system-type 'windows-nt))

;; Auto-update packages.
;;
(use-package auto-package-update
  :load-path "vendor/"
  :config
  (auto-package-update-maybe)
  :custom
  (auto-package-update-interval 1)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-prompt-before-update t)
  (apu--last-update-day-filename
   (no-littering-expand-var-file-name "auto-update-package-last-update-day")))

;; File settings
;;
(use-package files
  :ensure nil
  :custom
  (require-final-newline t)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 8)
  (kept-old-versions 4)
  (version-control t)
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (large-file-warning-threshold (* 20 1000 1000) "20 megabytes."))

;; Version control
(use-package vc-hooks
  :ensure nil
  :custom (vc-follow-symlinks t))

;; Global Modes
;;
;;; Enable which-key
(use-package which-key
  :delight
  :init (which-key-mode)
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 0.05)
  (which-key-setup-side-window-right-bottom))

;;; File Tree
(use-package all-the-icons
  :if window-system)
(use-package neotree
  :after (all-the-icons)
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-window-width 32)
  (neo-create-file-auto-open t)
  (neo-modern-sidebar t)
  (neo-point-auto-indent t)
  :general
  (general-nmap neotree-mode-map

    "RET" 'neotree-enter
    "TAB" 'neotree-stretch-toggle
    "q" 'neotree-hide
    "|" 'neotree-enter-vertical-split
    "-" 'neotree-enter-horizontal-split
    "'" 'neotree-quick-look
    "c" 'neotree-create-node
    "C" 'neotree-copy-node
    "d" 'neotree-delete-node
    "gr" 'neotree-refresh
    "H" 'neotree-select-previous-sibling-node
    "j" 'neotree-next-line
    "J" 'neotree-select-down-node
    "k" 'neotree-previous-line
    "K" 'neotree-select-up-node
    "L" 'neotree-select-next-sibling-node
    "q" 'neotree-hide
    "o" 'neotree-enter
    "r" 'neotree-rename-node
    "R" 'neotree-change-root
    "I" 'neotree-hidden-file-toggle))

;;; Ivy for completion
(use-package ivy
  :delight
  :init (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ive-use-selectable-prompt t))
;;; Ado-ado
(use-package counsel
  :config (progn
            (global-set-key (kbd "M-x") 'counsel-M-x)))

(use-package counsel-projectile
  :requires (counsel projectile)
  :config (counsel-projectile-mode))

;; Search regex
(use-package swiper)

(use-package flycheck
  :config (global-flycheck-mode))

(use-package flyspell
  :disabled ;; I don't like spell checking
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (not (eq system-type 'windows-nt))
  :delight
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra")))

(use-package flyspell-correct-ivy
  :requires (flyspell ivy))

;;; Resize all buffers at once with C-M-= / C-M--
(use-package default-text-scale
  :disabled
  :init (default-text-scale-mode))
;;; Restart Emacs
(use-package restart-emacs
  :commands restart-emacs)
;;; TODO Shackle to keep pop-up windows under control
;; (use-package shackle)
;;; TODO Workspaces
;; (use-package persp-mode)
;;; TODO workgroups
;; (use-package workgroups)

;;; Evil mode
(use-package evil
  :init (evil-mode 1)
  :custom
  (evil-want-C-u-scroll t))
(use-package evil-commentary
  :requires evil
  :init (evil-commentary-mode))
(use-package evil-surround
  :disabled
  :requires evil
  :init (global-evil-surround-mode))
(use-package evil-indent-textobject
  :disabled
  :requires evil)

;; Development Modes

;;; ALL
;;;
;;; Projectile
(use-package projectile
  :requires (ivy neotree)
  :delight '(:eval (concat " " (projectile-project-name)))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching nil)
  (projectile-switch-project-action 'neotree-projectile-action)
  :init
  (projectile-mode))
;;; Magit
(use-package magit
  :disabled
  :pin melpa-stable
  :custom
  (magit-commit-show-diff nil)
  :config (progn
            (put 'magit-clean 'disabled nil)
            (add-hook 'magit-status-sections-hook 'magit-insert-worktrees)))

;;; Company
;;; Auto-completion framework for most modes
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))
;;; EditorConfig
;;; Read files to set coding style options according to current project
(use-package editorconfig
  :disabled
  :config (editorconfig-mode 1))
;;; Rainbow Delimiters
;;; Highlight matching delimiters with unique colors.
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;;; Adapt to foreign indentation offsets
(use-package dtrt-indent
  :delight
  :custom (dtrt-indent-min-quality 60)
  :init (dtrt-indent-global-mode))
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))
(use-package adaptive-wrap
  :config (adaptive-wrap-prefix-mode))
(use-package whitespace
  :commands (whitespace-mode))
(use-package ws-butler
  :delight
  :config (ws-butler-global-mode))
;;; Editing
(use-package autorevert
  :ensure nil
  :delight auto-revert-mode
  :config (global-auto-revert-mode))
(use-package undo-tree
  :delight
  :custom (undo-tree-auto-save-history t)
  :config (global-undo-tree-mode))

(use-package unfill
  :disabled
  :bind ([remap fill-paragraph] . #'unfill-toggle))

;;; Other Modes
;;;
;;; Markdown Mode
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config (progn
            (add-hook 'markdown-mode-hook 'visual-line-mode)
            (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))))
;;; JSON Formatter
(use-package json-mode
  :mode "\\.json$")
;;; Dockerfile
(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")
;;; YAML mode
(use-package yaml-mode
  :mode "\\.ya?ml\'")
;;; Git Attributes
(use-package gitattributes-mode
  :disabled
  :mode ("/\\.gitattributes\\'"
         "/info/attributes\\'"
         "/git/attributes\\'"))
(use-package gitconfig-mode
  :disabled
  :mode ("/\\.gitconfig\\'"
         "/\\.git/config\\'"
         "/modules/.*/config\\'"
         "/git/config\\'"
         "/\\.gitmodules\\'"
         "/etc/gitconfig\\'"))
(use-package gitignore-mode
  :disabled
  :mode ("/\\.gitignore\\'"
         "/info/exclude\\'"
         "/git/ignore\\'"))

(use-package dired
  :ensure nil
  :commands (dired)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired` buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired`.")
  (dired-recursive-deletes 'top)
  (delete-by-moving-to-trash t)
  (dired-use-ls-dired nil))
;; TODO: do I want emmet mode?
(use-package emmet-mode
  :disabled
  :custom (emmet-move-cursor-between-quotes t)
  :config (add-hook 'css-mode-hook  'emmet-mode))

;; Appearance
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (horizontal-scroll-bar-mode 0)))

;;; Theme
(use-package powerline
  :init (powerline-default-theme))
(use-package doom-themes
  :init (load-theme 'doom-molokai t)
  :config
  (progn
    (doom-themes-org-config)))
;;; Font
(set-face-attribute 'default nil
                    :family "Fira Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)
;;; Highlight TODOs
(use-package hl-todo
  :init (global-hl-todo-mode))
;;; Better scrolling
(use-package smooth-scroll
  :if (eq system-type 'gnu/linux);;(display-graphic-p)
  :delight
  :custom (smooth-scroll/vscroll-step-size 8)
  :init (progn
          (require 'smooth-scroll)
          (smooth-scroll-mode 1)))

(use-package recentf
  :ensure nil
  :custom (recentf-max-saved-items 256)
  :config (recentf-mode))


;;; Fix Annoyances

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(defalias 'yes-or-no-p 'y-or-n-p)

(customize-set-variable 'cua-mode t)
(customize-set-variable 'visible-bell nil)
(customize-set-variable 'blink-matching-paran nil)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'sentence-end-double-space nil)
(customize-set-variable 'ring-bell-function (lambda ()
                                              (invert-face 'mode-line)
                                              (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;;; provide init package
(provide 'init)

;;; init.el ends here
