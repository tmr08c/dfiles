;; init --- Justin Smestad's Emacs init file
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

;; Used to benchmark init timings
(use-package benchmark-init
  :disabled
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package delight)
(use-package dash)

;; Ensure system has required packages and install if missing
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables '("PATH" "GOPATH" "PGHOST"))
  :config
  (exec-path-from-shell-initialize))
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

(use-package recentf
  :ensure nil
  :requires no-littering
  :custom
  (recentf-auto-cleanup 200)
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup 'never)
  (recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))
  :config
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


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

    "SPC" '(counsel-M-x :which-key "M-x")
    ;; "TAB" '(switch-to-other-buffer :which-key "prev buffer")

    ;;; Help bindings
    "?" '(counsel-descbinds :which-key "Help")
    "h" '(:ignore t :which-key "Help")
    "h d f" '(counsel-describe-function :which-key "describe function")
    "h d m" '(describe-mode :which-key "describe modes") ;; TODO: https://framagit.org/steckerhalter/discover-my-major
    "h d v" '(counsel-describe-variable :which-key "describe variable")

    ;;; Buffers
    "b"   '(:ignore t :which-key "Buffers")
    "b b" '(ivy-switch-buffer :which-key "list buffers")
    "b n" '(next-buffer :which-key "next buffer")
    "b p" '(previous-buffer :which-key "prev buffer")
    "b d" '((lambda ()
              (interactive)
              (kill-buffer (current-buffer)))
            :which-key "close current buffer")
    "b s" '((lambda ()
              (interactive)
              (switch-to-buffer (get-buffer-create "*scratch*")))
            :which-key "scratch buffer")

    ;;; Files
    "f"   '(:ignore t :which-key "Files")
    "f D" '((lambda ()
              (interactive)
              (let ((filename (buffer-file-name))
                    (buffer (current-buffer))
                    (name (buffer-name)))
                (if (not (and filename (file-exists-p filename)))
                    (ido-kill-buffer)
                  (when (yes-or-no-p "Are you sure you want to delete this file? ")
                    (delete-file filename t)
                    (kill-buffer buffer)
                    (message "File '%s' successfully removed" filename)))))
            :which-key "delete file and kill buffer")
    "f f" '(counsel-find-file :which-key "find file")
    "f t" '(neotree-toggle :which-key "toggle file tree")
    "f e d" '((lambda ()
                (interactive)
                (find-file-existing user-init-file))
              :which-key "open emacs configuration")


    "d" '(:ignore t :which-key "Docs")
    "d d" '((lambda ()
              (interactive)
              (counsel-dash
               (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (substring-no-properties (or (thing-at-point 'symbol) "")))))
            :which-key "Lookup thing at point")
    "d D" '(counsel-dash :which-key "Lookup thing at point with docset")


    "g" '(:ignore t :which-key "Go to")
    "g d" '(dumb-jump-go :which-key "definition")
    "g D" '(dumb-jump-go-other-window :which-key "definition (other window)")


    ;;; Projects
    "p"   '(:ignore t :which-key "Projects")
    "p !" '(projectile-run-shell-command-in-root :which-key "run command")
    "p %" '(projectile-replace-regexp :which-key "replace regexp")
    ;; "p a" '(projectile-toggle-between-implementation-and-test :which-key "toggle test")
    "p I" '(projectile-invalidate-cache :which-key "clear cache")
    "p R" '(projectile-replace :which-key "replace")
    "p b" '(counsel-projectile-switch-to-buffer :which-key "switch to buffer")
    "p d" '(counsel-projectile-find-dir :which-key "find directory")
    "p f" '(counsel-projectile-find-file :which-key "open file")
    "p k" '(projectile-kill-buffers :which-key "kill buffers")
    "p p" '(counsel-projectile-switch-project :which-key "open project")
    "p r" '(projectile-recentf :which-key "recent files")
    "p t" '(neotree-projectile-action :which-key "project tree")

    ;; Does not seem to work
    ;; "p s" '(counsel-projectile-rg :which-key "search in project")

    ;;; Quit
    "q"   '(:ignore t :which-key "Quit")
    "q q" '(kill-emacs :which-key "quit")
    "q r" '(restart-emacs :which-key "restart")

    ;;; Search
    "s" '(:ignore t :which-key "Search")
    "s s" '(swiper :which-key "search buffer")
    "s S" '(lambda ()
             (interactive)
             (let ((input (if (region-active-p)
                              (buffer-substring-no-properties
                               (region-beginning) (region-end))
                            (thing-at-point 'symbol t))))
               (swiper input))
             :which-key "search buffer")

    ;;; Themes
    "t" '(:ignore t :which-key "Theme")
    "t s" '(counsel-load-theme :which-key "switch theme")

    ;;; Windows
    "w"   '(:ignore t :which-key "Windows")
    "w d" '(delete-window :which-key "close window")
    "w /" '((lambda ()
              (interactive)
              (split-window-horizontally)
              (other-window 1))
            :which-key "split vertical")
    "w -" '((lambda ()
              (interactive)
              (split-window-vertically)
              (other-window 1))
            :which-key "split horizontal")
    "w h" '(evil-window-left :which-key "window left")
    "w <left>" '(evil-window-left :which-key nil)
    "w j" '(evil-window-down :which-key "window down")
    "w <down>" '(evil-window-down :which-key nil)
    "w k" '(evil-window-up :which-key "window up")
    "w <up>" '(evil-window-up :which-key nil)
    "w l" '(evil-window-right :which-key "window right")
    "w <right>" '(evil-window-right :which-key nil)
    "w =" '(balance-windows :which-key "balance window split")))

;; Platform
;;
(use-package linux
  :load-path "vendor/"
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
(use-package vlf
  :hook csv-mode)
(use-package files
  :ensure nil
  :demand t
  :custom
  (backup-by-copying t)
  (require-final-newline t)
  (delete-old-versions t)
  (version-control t)
  (backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (large-file-warning-threshold (* 20 1000 1000) "20 megabytes."))

;; Version control
(use-package vc-hooks
  :ensure nil
  :demand t
  :custom (vc-follow-symlinks t))

;; Global Modes
;;
;;; ace-window (for better window switching)
(use-package ace-window
  :disabled)
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
(use-package neotree
  :after all-the-icons
  :custom
  ;; (neo-autorefresh t) ;; annoyingly asks to change root all the time!
  (neo-create-file-auto-open t)
  (neo-modern-sidebar t)
  (neo-point-auto-indent t)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-window-fixed-size nil)
  (neo-window-width 25)
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

(use-package smex
  :init (smex-initialize))
;;; Ivy for completion
(use-package ivy
  :delight
  :init (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ive-use-selectable-prompt t))
(use-package all-the-icons-ivy
  :disabled
  :ensure t
  :after (ivy projectile)
  :custom
  (all-the-icons-ivy-buffer-commands
   '(ivy-switch-buffer
     ivy-switch-buffer-other-window
     counsel-projectile-switch-to-buffer))
  (all-the-icons-ivy-file-commands
   '(counsel-find-file
     counsel-file-jump
     counsel-recentf
     counsel-projectile-find-file
     counsel-projectile-find-dir))
  :config
  (all-the-icons-ivy-setup))

;;; Ado-ado
(use-package counsel
  :general
  (general-define-key
   "M-x" 'counsel-M-x))

(use-package counsel-projectile
  :requires (counsel projectile rg)
  :config (counsel-projectile-mode))

(use-package counsel-dash
  :defer t
  :custom
  (counsel-dash-browser-func 'eww)
  (counsel-dash-common-docsets '("Ruby"))
  :hook
  ((emacs-lisp-mode . (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
   (ruby-mode . (lambda () (setq-local counsel-dash-docsets '("Ruby"))))))

(use-package rg
  :commands (rg rg-project rg-dwim rg-literal))

;; Search regex
(use-package swiper
  :if (eq system-type 'darwin)
  :general
  (general-define-key
   "M-s" 'swiper))

(use-package swiper
  :unless (eq system-type 'darwin)
  :general
  (general-define-key
   "C-s" 'swiper))

(use-package flycheck
  :custom
  (flycheck-rubocop-lint-only t)
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-disabled-checkers '(ruby-rubylint))
  :hook (prog-mode . flycheck-mode))
(use-package flycheck-pos-tip
  :disabled
  :defer 2
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flyspell
  ;; :disabled ;; I don't like spell checking
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (not (eq system-type 'windows-nt))
  :defer 2
  :delight
  ;; :hook ((text-mode . flyspell-mode)
  ;;        (prog-mode . flyspell-prog-mode))
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
(use-package popwin
  :defer t
  :hook (after-init . popwin-mode))

(use-package js-editing
  :load-path "vendor/")

;; Development Modes

;;; ALL
;;;
;;; Projectile
(use-package projectile
  :defer t
  :requires ivy
  :delight ;;'(:eval (concat " " (projectile-project-name)))
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  (projectile-enable-caching nil)
  (projectile-switch-project-action 'counsel-projectile-find-file)
  (projectile-sort-order 'recentf)
  :config
  (add-to-list 'projectile-project-root-files ".clang_complete")
  (projectile-mode))
;;; Magit
(use-package magit
  :defer t)
(use-package magithub
  :disabled
  :after magit
  :config
  (magithub-feature-autoinject t))
;; May not be needed:
;; :custom
;; (magit-commit-show-diff nil)
;; :hook (magit-status-sections . magit-insert-worktrees)
;; :config
;; (put 'magit-clean 'disabled nil))

(use-package js-completion
  :load-path "vendor/")

(use-package emojify
  :defer t
  :init
  (progn
    (setq emojify-display-style 'unicode)
    (global-emojify-mode)))

;;; EShell
(use-package eshell
  :commands eshell
  :custom
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-output 'this)
  (eshell-scroll-to-bottom-on-input 'all))

(use-package repl-toggle
  :disabled
  :custom
  (rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (ruby-mode . inf-ruby)
     (js2-mode . nodejs-repl)
     (rjsx-mode . nodejs-repl)))
  :config
  (repl-toggle-mode))

;;; direnv
(use-package direnv
  :defer 2
  :ensure-system-package direnv)
;;; EditorConfig
;;; Read files to set coding style options according to current project
(use-package editorconfig
  :disabled
  :config (editorconfig-mode 1))

;; Ruby
(use-package js-ruby
  :load-path "vendor/")

;; HTML / CSS
(use-package js-web
  :load-path "/vendor")

;; Javascript
(use-package js-javascript
  :load-path "/vendor")

;; Shell
(use-package sh-mode
  :ensure nil
  :mode
  (("\\.zshrc" . sh-mode)
   ("bashrc$" . sh-mode)
   ("bash_profile$" . sh-mode)
   ("bash_aliases$" . sh-mode)
   ("bash_local$" . sh-mode)
   ("bash_completion$" . sh-mode)))

;; C (via irony-mode)
(use-package irony
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode))
  :config
  (progn
    (setq irony-additional-clang-options '("-std=c++11"))
    (setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                                    iron-cdb-libclang))

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))
(use-package irony-eldoc
  :hook (irony-mode . irony-eldoc))
(use-package flycheck-irony
  :hook (irony-mode . flycheck-irony-setup))
;; (use-package lsp-clangd
;;   :load-path "/vendor"
;;   :hook ((c-mode . lsp-clangd-c-enable)
;;          (c++-mode . lsp-clangd-c++-enable)
;;          (objc-mode . lsp-clangd-objc-enable)))
(use-package platformio-mode
  :after irony-mode
  :hook ((c-mode . platformio-conditionally-enable)
         (c++-mode . platformio-conditionally-enable)))
(use-package clang-format
  :config
  (defun c-mode-before-save-hook ()
    (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
      (call-interactively 'clang-format)))

  (add-hook 'before-save-hook #'c-mode-before-save-hook))
(use-package arduino-mode
  :after irony
  :config
  (add-to-list 'irony-supported-major-modes 'arduino-mode)
  (add-to-list 'irony-lang-compile-option-alist '(arduino-mode . "c++")))

;; Go
(use-package js-golang
  :load-path "vendor/")

;; Elisp
(use-package eldoc
  :ensure nil
  :delight)

;; Erlang
(use-package erlang
  :mode "\\.erl$")

;; Elixir
(use-package elixir-mode
  :commands elixir-mode
  :mode "\\.exs?")
(use-package alchemist
  :commands alchemist-mode
  :hook (elixir-mode . alchemist-mode))
(use-package flycheck-mix
  :hook (elixir-mode . flycheck-mix-setup))

;; Python
(use-package python
  :mode ("\\.py" . python-mode))
(use-package anaconda-mode
  :hook python-mode)
(use-package pyenv-mode
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions)
  :hook python-mode)
(use-package lsp-python
  :after lsp-mode
  :hook (python-mode . lsp-python-enable))


;; Scala
(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode))
(use-package ensime
  :hook (scala-mode . ensime-mode))
(use-package sbt-mode
  :hook (scala-mode . sbt-mode))



;; Markdown Mode
(use-package markdown-mode
  :mode "\\.md$"
  :hook (markdown-mode . flyspell-mode))

;; CSV
(use-package csv-mode
  :mode "\\.csv$"
  :config

  (defun csv-align-visible ()
    "Align only visible entries in csv-mode."
    (interactive)
    (csv-align-fields nil (window-start) (window-end)))

  ;; C-c C-a is already bound to align all fields, but can be too slow.
  :bind (:map csv-mode-map
              ("C-c C-w" . 'csv-align-visible)))

;; JSON Formatter
(use-package json-mode
  :custom
  (js-indent-level 2)
  :mode ("\\.json$"
         "\\.jshintrc$"))
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
  :demand t
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
      (menu-bar-mode 0)
      (horizontal-scroll-bar-mode 0)

      ;; Theme Emacs for dark color scheme
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))

      (add-hook 'after-init-hook 'set-frame-size-according-to-resolution)
      (add-hook 'after-make-frame-functions 'set-frame-size-according-to-resolution)))


(defun set-frame-size-according-to-resolution (&rest frame)
  "Set FRAME height to screen height and width to half total."
  (if window-system
      (let ((f (if (car frame)
		               (car frame)
	               (selected-frame))))
        (progn
          (set-frame-height f (display-pixel-height) nil 'pixelwise)
          (set-frame-width f (/ (display-pixel-width) 2) nil 'pixelwise)
          (set-frame-position f 0 0)))))

;; Modeline
(use-package shrink-path)
(use-package eldoc-eval)
(use-package doom-modeline
  :requires (eldoc-eval shrink-path)
  :load-path "vendor/"
  :hook (after-init . doom-modeline-init))
(use-package hide-mode-line
  :hook ((neotree-mode . hide-mode-line-mode)
         (completion-list-mode . hide-mode-line-mode)
         (completion-in-region-mode . hide-mode-line-mode)))
;; (use-package telephone-line
;;   :hook (after-init . telephone-line-mode))
;; (use-package powerline
;; :hook (after-init . powerline-reset)
;; )
;; (use-package powerline-evil
;;   :requires powerline
;;   :init (powerline-evil-vim-color-theme))

(use-package all-the-icons
  :if window-system)

(use-package doom-themes
  :init (load-theme 'doom-molokai t)
  ;; :custom
  ;; (doom-neotree-file-icons t)
  ;; (doom-themes-org-config)
  :config
  (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config)
  (doom-themes-org-config))

;; Font
(set-face-attribute 'default nil
                    :family "Fira Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Highlight TODOs
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

;; Better scrolling
(use-package pixel-scroll
  :disabled
  :ensure nil
  :if (> emacs-major-version 25)
  :hook (after-init . pixel-scroll-mode))

;; Line Numbers
(use-package display-line-numbers
  :ensure nil
  :if (> emacs-major-version 25)
  :hook (prog-mode . display-line-numbers-mode))

;; Fix Annoyances
(use-package uniquify
  :ensure nil
  :demand t
  :custom (uniquify-buffer-name-style 'forward))

(defalias 'yes-or-no-p 'y-or-n-p)

;; Use Github as the standard
;; ref http://hilton.org.uk/blog/source-code-line-length
(customize-set-variable 'fill-column 125)

(customize-set-variable 'byte-compile-warnings nil)
(customize-set-variable 'blink-matching-paran nil)
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'cua-mode t)
(customize-set-variable 'desktop-save-mode nil)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'initial-major-mode 'markdown-mode)
(customize-set-variable 'initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string)))
(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'sentence-end-double-space nil)
(customize-set-variable 'visible-bell nil)
(customize-set-variable 'ring-bell-function (lambda ()
                                              (invert-face 'mode-line)
                                              (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(provide 'init)
;;; init.el ends here
