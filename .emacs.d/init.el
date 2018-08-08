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
  (exec-path-from-shell-variables '("SHELL" "MANPATH" "PATH" "GOPATH" "PGHOST" "SSH_AUTH_SOCK"))
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

(customize-set-variable 'user-full-name "Justin Smestad")
(customize-set-variable 'user-mail-address "justin.smestad@gmail.com")

(use-package js-ui
  :load-path "vendor/")

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
    "p T" '(doom/ivy-tasks :which-key "List project tasks")

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
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-add-column-padding 1)
  (which-key-side-window-max-width 0.33)
  ;; (which-key-idle-delay 0.05)
  (which-key-setup-side-window-right-bottom))

;;; File Tree
(use-package neotree
  :after all-the-icons
  :custom
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
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  (ivy-display-style 'fancy)
  (ivy-format-function 'ivy-format-function-line)
  (ivy-use-selectable-prompt t))
(use-package doom-todo-ivy
  :after ivy
  :load-path "vendor/")
(use-package ivy-rich
  :disabled
  :load-path "vendor/"
  ;; :defer 2
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1))

;;; Ado-ado
(use-package counsel
  :general
  (general-define-key
   "M-x" 'counsel-M-x))

(use-package counsel-projectile
  :load-path "vendor/"
  :after projectile
  :hook (counsel-mode . counsel-projectile-mode))

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
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flyspell
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (not (eq system-type 'windows-nt))
  :defer 2
  :delight
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
  :after ivy
  :delight ;;'(:eval (concat " " (projectile-project-name)))
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  (projectile-enable-caching nil)
  (projectile-switch-project-action 'counsel-projectile-find-file)
  (projectile-sort-order 'recentf)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-project-root-files ".clang_complete")
  (projectile-mode +1))

(use-package js-completion
  :load-path "vendor/")

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

;; Other modes
(use-package js-altmodes
  :load-path "/vendor")


;; Go
(use-package js-golang
  :load-path "vendor/")

;; Scala
(use-package js-scala
  :load-path "vendor/")

;; C / C++ / Arduino
(use-package js-clang
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


;; TODO: do I want emmet mode?
(use-package emmet-mode
  :disabled
  :custom (emmet-move-cursor-between-quotes t)
  :config (add-hook 'css-mode-hook  'emmet-mode))


;; Highlight TODOs
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

;; Adjust the built-in Emacs packages
(use-package js-builtin
  :load-path "vendor/")

;; Version Control (git and what-not)
(use-package js-vc
  :load-path "vendor/")

(use-package js-org
  :load-path "vendor/")

(defalias 'yes-or-no-p 'y-or-n-p)

(customize-set-variable 'byte-compile-warnings nil)
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'cua-mode t)
(customize-set-variable 'desktop-save-mode nil)
(customize-set-variable 'indent-tabs-mode nil)
;; (customize-set-variable 'initial-major-mode 'markdown-mode)
(customize-set-variable 'initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string)))
(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'sentence-end-double-space nil)

;; Platform Specific
(use-package linux
  :load-path "vendor/"
  :if (eq system-type 'gnu/linux))
(use-package osx
  :load-path "vendor/"
  :if (eq system-type 'darwin))

(provide 'init)
;;; init.el ends here
