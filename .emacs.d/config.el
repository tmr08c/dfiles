;;; config.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require '+funcs)

(defvar +completion-engine 'ivy
  "Setting to control whether to use helm or ivy.")

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package which-key
  :defer 1
  :delight
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-max-width 0.33)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(use-package general
  :demand
  :functions space-leader-def
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-auto-unbind-keys)
  (general-create-definer space-leader-def
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-evil-setup))

(use-package evil
  :demand
  :init (setq evil-want-C-u-scroll t
              evil-want-integration t
              evil-want-keybinding nil) ; This MUST be in init.
  :config
  (setq evil-want-visual-char-semi-exclusive t
        evil-want-Y-yank-to-eol t
        evil-shift-width 2
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        evil-respect-visual-line-mode t
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil
        ;; cursor appearance
        ;; evil-default-cursor '+evil-default-cursor
        evil-normal-state-cursor 'box
        ;; evil-emacs-state-cursor  '(box +evil-emacs-cursor)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Disable Evil for the states below
  (evil-set-initial-state 'paradox-menu-mode 'emacs)

  (defun +evil|update-shift-width ()
    (setq evil-shift-width tab-width))
  (add-hook 'after-change-major-mode-hook #'+evil|update-shift-width t)

  (evil-mode 1))

(use-package evil-escape
  :requires evil
  :init (evil-escape-mode 1)
  :delight
  :custom (evil-escape-delay 0.2))
(use-package evil-surround
  :defer 5
  :init (global-evil-surround-mode 1))
(use-package evil-matchit
  :defer 5
  :init (global-evil-matchit-mode))
(use-package evil-goggles
  :defer 5
  :delight
  :config
  (setq evil-goggles-duration 0.1
        evil-goggles-enable-delete nil)
  :init
  (evil-goggles-mode))
(use-package evil-easymotion
  :defer 5
  :delight)
(use-package evil-commentary
  :defer t
  :delight
  :init (evil-commentary-mode))
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))


(use-package editorconfig
  :defer t
  :config (editorconfig-mode 1))

(use-package eyebrowse ; Easy workspaces creation and switching
  :demand
  :delight
  :config
  (setq eyebrowse-new-workspace t)
  (defhydra eyebrowse-hydra (:color blue)
    "
^
^Eyebrowse^         ^Do^                ^Switch^
^─────────^─────────^──^────────────────^──────^────────────
_q_ quit            _c_ create          _<_ previous
^^                  _k_ kill            _>_ next
^^                  _r_ rename          _e_ last
^^                  ^^                  _s_ switch
^^                  ^^                  ^^
"
    ("q" nil)
    ("<" eyebrowse-prev-window-config :color red)
    (">" eyebrowse-next-window-config :color red)
    ("c" eyebrowse-create-window-config)
    ("e" eyebrowse-last-window-config)
    ("k" eyebrowse-close-window-config :color red)
    ("r" eyebrowse-rename-window-config)
    ("s" eyebrowse-switch-to-window-config))
  ;;  (defhydra eyebrowse-hydra (:hint nil :color red)
  ;;    "\n\n
  ;; Go to^^^^^^                         Actions^^
  ;; ─────^^^^^^───────────────────────  ───────^^──────────────────────
  ;; [_0_.._9_]^^     nth/new workspace  [_d_] close current workspace
  ;; [_<tab>_]^^^^    last workspace     [_R_] rename current workspace
  ;; [_c_/_C_]^^      create workspace
  ;; [_n_/_C-l_]^^    next workspace
  ;; [_p_/_C-h_]  prev workspace\n
  ;; [_w_]^^^^       workspace w/helm/ivy\n"
  ;;    ;; ("?" spacemacs//workspaces-ts-toggle-hint)
  ;;    ("0" eyebrowse-switch-to-window-config-0 :exit t)
  ;;    ("1" eyebrowse-switch-to-window-config-1 :exit t)
  ;;    ("2" eyebrowse-switch-to-window-config-2 :exit t)
  ;;    ("3" eyebrowse-switch-to-window-config-3 :exit t)
  ;;    ("4" eyebrowse-switch-to-window-config-4 :exit t)
  ;;    ("5" eyebrowse-switch-to-window-config-5 :exit t)
  ;;    ("6" eyebrowse-switch-to-window-config-6 :exit t)
  ;;    ("7" eyebrowse-switch-to-window-config-7 :exit t)
  ;;    ("8" eyebrowse-switch-to-window-config-8 :exit t)
  ;;    ("9" eyebrowse-switch-to-window-config-9 :exit t)
  ;;    ("C-0" eyebrowse-switch-to-window-config-0)
  ;;    ("C-1" eyebrowse-switch-to-window-config-1)
  ;;    ("C-2" eyebrowse-switch-to-window-config-2)
  ;;    ("C-3" eyebrowse-switch-to-window-config-3)
  ;;    ("C-4" eyebrowse-switch-to-window-config-4)
  ;;    ("C-5" eyebrowse-switch-to-window-config-5)
  ;;    ("C-6" eyebrowse-switch-to-window-config-6)
  ;;    ("C-7" eyebrowse-switch-to-window-config-7)
  ;;    ("C-8" eyebrowse-switch-to-window-config-8)
  ;;    ("C-9" eyebrowse-switch-to-window-config-9)
  ;;    ("<tab>" eyebrowse-last-window-config)
  ;;    ("<return>" nil :exit t)
  ;;    ("TAB" eyebrowse-last-window-config)
  ;;    ("RET" nil :exit t)
  ;;    ("c" eyebrowse-create-window-config :exit t)
  ;;    ("C" eyebrowse-create-window-config)
  ;;    ("C-h" eyebrowse-prev-window-config)
  ;;    ("C-l" eyebrowse-next-window-config)
  ;;    ("d" eyebrowse-close-window-config)
  ;;    ("n" eyebrowse-next-window-config)
  ;;    ("p" eyebrowse-prev-window-config)
  ;;    ("R" eyebrowse-rename-window-config :exit t)
  ;;    ("w" eyebrowse-switch-to-window-config :exit t))
  (eyebrowse-mode t))

(use-package projectile
  :commands (projectile-run-shell-command-in-root
             projectile-replace-regexp
             projectile-toggle-between-implementation-and-test
             projectile-invalidate-cache
             projectile-replace
             projectile-kill-buffers
             projectile-recentf
             projectile-ag
             projectile-find-file
             projectile-find-dir
             projectile-switch-project)
  :config
  (progn
    (setq projectile-indexing-method 'alien
          projectile-enable-caching nil
          projectile-switch-project-action 'projectile-find-file
          projectile-sort-order 'recentf)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (add-to-list 'projectile-project-root-files ".clang_complete")
    (projectile-mode +1)))

(use-package direnv
  :defer 2
  :ensure-system-package direnv)

(use-package amx
  :hook (after-init . amx-initialize))

;; Company
(use-package company
  :defer t
  :delight
  :defines company-backends
  :hook (after-init . global-company-mode)
  :config
  ;; (company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-echo-delay 0 ; remove annoying blinking
        company-idle-delay 0.6
        company-minimum-prefix-length 2
        company-require-match 'never
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-limit 14
        company-global-modes
        '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-transformers '(company-sort-by-occurrence)
        company-backends '()))
(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (prescient-persist-mode +1))
(use-package flx)
(use-package company-flx
  :hook (company-mode . company-flx-mode))
(use-package company-posframe
  :hook (company-mode . company-posframe-mode))

;; Language Server Protocol (LSP)
(use-package lsp-mode
  :commands lsp
  :hook ((ruby-mode
          js-mode js2-mode
          typescript-mode
          python-mode
          web-mode
          css-mode
          elixir-mode
          go-mode) . lsp)
  :config
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil)
  (add-to-list 'exec-path "~/code/github/elixir-ls/release"))
(use-package company-lsp)
(use-package lsp-ui
  :custom-face
  (lsp-ui-doc-background ((t `(:background nil))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-border (face-foreground 'default)

        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))
;; (use-package dap-mode)

(use-package smartparens
  :defer 2
  :config
  (progn
    (require 'smartparens-config)
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-show-pair-delay 0.1
          sp-max-pair-length 4
          sp-max-prefix-length 50
          sp-escape-quotes-after-insert nil)

    ;; Smartparens' navigation feature is neat, but does not justify how expensive
    ;; it is. It's also less useful for evil users. This may need to be
    ;; reactivated for non-evil users though. Needs more testing!
    (defun js|disable-smartparens-navigate-skip-match ()
      (setq sp-navigate-skip-match nil
            sp-navigate-consider-sgml-tags nil))
    (add-hook 'after-change-major-mode-hook #'js|disable-smartparens-navigate-skip-match)

    ;; autopairing in `eval-expression' and `evil-ex'
    (defun js|init-smartparens-in-eval-expression ()
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression' or
  `evil-ex'."
      (when (memq this-command '(eval-expression evil-ex))
        (smartparens-mode)))
    (add-hook 'minibuffer-setup-hook #'js|init-smartparens-in-eval-expression)
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    (defun js|smartparens-pair-newline (id action context)
      (save-excursion
        (newline)
        (indent-according-to-mode)))

    (defun js|smartparens-pair-newline-and-indent (id action context)
      (js|smartparens-pair-newline id action context)
      (indent-according-to-mode))

    ;; smartparens breaks evil-mode's replace state
    (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
    (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)
    (smartparens-global-mode +1)))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dtrt-indent
  :defer t
  :delight
  :custom (dtrt-indent-min-quality 60)
  :init (dtrt-indent-global-mode))

(use-package indent-guide
  :commands (indent-guide-mode indent-guide-global-mode)
  :custom (indent-guide-delay 0.3))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (css-mode . aggressive-indent-mode)))

(use-package adaptive-wrap
  :defer t
  :config (adaptive-wrap-prefix-mode))

(use-package dumb-jump
  :commands (dump-jump-go
             dumb-jump-go-other-window
             dump-jump-go-prompt
             dump-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window)
  ;; TODO FIXME this needs to swap to helm based on +configuration
  :custom (dumb-jump-selector 'ivy))

(use-package whitespace
  :defer 5
  :config
  (setq whitespace-line-column nil
        whitespace-style
        '(face indentation tabs tab-mark spaces space-mark newline newline-mark
               trailing lines-tail)
        whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))
  (defun doom|disable-whitespace-mode-in-childframes (frame)
    "`whitespace-mode' inundates child frames with whitspace markers, so disable
it to fix all that visual noise."
    (when (frame-parameter frame 'parent-frame)
      (with-selected-frame frame
        (setq-local whitespace-style nil)
        frame)))
  (add-hook 'after-make-frame-functions #'doom|disable-whitespace-mode-in-childframes)
  (defun doom|highlight-non-default-indentation ()
    "Highlight whitespace that doesn't match your `indent-tabs-mode' setting."
    (unless (or (bound-and-true-p global-whitespace-mode)
                (bound-and-true-p whitespace-mode)
                (eq indent-tabs-mode (default-value 'indent-tabs-mode))
                (eq major-mode 'fundamental-mode)
                (derived-mode-p 'special-mode))
      (require 'whitespace)
      (set (make-local-variable 'whitespace-style)
           (if (or (bound-and-true-p whitespace-mode)
                   (bound-and-true-p whitespace-newline-mode))
               (cl-union (if indent-tabs-mode '(tabs tab-mark) '(spaces space-mark))
                         whitespace-style)
             `(face ,@(if indent-tabs-mode '(tabs tab-mark) '(spaces space-mark))
                    trailing-lines tail)))
      (whitespace-mode +1)))

  (add-hook 'after-change-major-mode-hook #'doom|highlight-non-default-indentation))

(use-package ws-butler
  :delight
  :defer t
  :config
  (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
                '(special-mode comint-mode term-mode eshell-mode)))
  (ws-butler-global-mode))

(use-package autorevert
  :ensure nil
  :defer t
  :delight auto-revert-mode
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package undo-tree
  :delight
  :custom
  (undo-tree-auto-save-history nil)
  :hook (evil-mode . global-undo-tree-mode))

(use-package unfill
  :disabled
  :bind ([remap fill-paragraph] . #'unfill-toggle))

(use-package outshine
  :hook (prog-mode . outshine-mode))
(use-package hideshow
  :functions hs-toggle-hiding
  :ensure nil
  :delight
  :config
  (progn
    (defun toggle-fold ()
      (interactive)
      (save-excursion
        (end-of-line)
        (hs-toggle-hiding))))
  :hook (prog-mode . hs-minor-mode))

(use-package hide-comnt
  :load-path "vendor/"
  :commands hide/show-comments-toggle)

(use-package yasnippet
  :defer 5
  :hook ((text-mode prog-mode snippet-mode) . yas-minor-mode-on)
  :commands (yas-minor-mode yas-minor-mode-on yas-expand yas-expand-snippet
                            yas-lookup-snippet yas-insert-snippet yas-new-snippet
                            yas-visit-snippet-file snippet-mode)
  :config
  (setq yas-also-auto-indent-first-line t
        yas-triggers-in-field t) ; Allow nested snippets

  ;; fix an error caused by smartparens interfering with yasnippet bindings
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)

  ;; Exit snippets on ESC from normal mode
  (add-hook '+evil-esc-hook #'yas-exit-all-snippets))

(use-package hl-todo
  :defer 10
  :init (global-hl-todo-mode))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :custom (highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package highlight-indentation
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode))

(use-package visual-fill-column
  :config
  (setq-default
   visual-fill-column-center-text t
   visual-fill-column-width
   ;; take Emacs 26 line numbers into account
   (+ (if (boundp 'display-line-numbers) 6 0)
      fill-column)))

(use-package swiper
  :after evil
  :config
  (setq swiper-action-recenter t)
  :general
  (general-define-key
   "C-s" 'swiper))

(use-package rg
  :commands (rg rg-project rg-dwim rg-literal))

(use-package midnight
  :defer 10)

(use-package google-translate
  :commands (spacemacs/set-google-translate-languages
             google-translate-query-translate
             google-translate-at-point
             google-translate-query-translate-reverse
             google-translate-at-point-reverse)
  :init
  (progn
    (defun spacemacs/set-google-translate-languages (source target)
      "Set source language for google translate.
For instance pass En as source for English."
      (interactive
       "sEnter source language (ie. en): \nsEnter target language (ie. en): "
       source target)
      (message
       (format "Set google translate source language to %s and target to %s"
               source target))
      (setq google-translate-default-source-language (downcase source))
      (setq google-translate-default-target-language (downcase target))))
  :config
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-enable-ido-completion t
          google-translate-show-phonetic t
          google-translate-default-source-language "en"
          google-translate-default-target-language "de")))

;; Golang
(use-package go-mode
  :mode "\\.go\\'"
  :requires (company)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun my-go-mode-hook-fn ()
    (go-eldoc-setup)
    ;; (set (make-local-variable 'company-backends) '(company-go))
    (setq-local company-backends '(company-go))
    (setq tab-width 2
          indent-tabs-mode 1)
    (flycheck-gometalinter-setup)
    (flycheck-mode 1))
  (add-hook 'go-mode-hook #'my-go-mode-hook-fn)
  :custom
  (gofmt-command "goimports"))
(use-package go-eldoc
  :commands go-eldoc-setup)
(use-package flycheck-gometalinter
  :commands flycheck-gometalinter-setup
  ;; :hook (go-mode . flycheck-gometalinter-setup)
  :custom
  ;; skip linting for vendor dirs
  (flycheck-gometalinter-vendor t)
  ;; use in test files
  (flycheck-gometalinter-test t)
  ;; only use fast linters
  (flycheck-gometalinter-fast t)
  ;; explicitly disable 'gotype' & 'govet' linters (also currently broken Nix overlays)
  (flycheck-gometalinter-disable-linters
   '("gosec" "gotype" "vet" "vetshadow" "megacheck" "interfacer" "ineffassign")))
(use-package go-projectile
  :hook (go-mode . go-projectile-mode))
(use-package go-gen-test
  :commands (go-gen-test-exported
             go-gen-test-all
             go-gen-test-dwim))
(use-package go-fill-struct
  :commands (go-fill-struct))
(use-package godoctor
  :commands (godoctor-godoc
             godoctor-extract
             godoctor-rename
             godoctor-toggle))
(use-package go-rename
  :commands  go-rename)
(use-package go-impl
  :commands go-impl)


(use-package ruby-mode
  :ensure nil
  :ensure-system-package
  ((ruby-lint   . "gem install ruby-lint")
   (ripper-tags . "gem install ripper-tags")
   (pry . "gem install pry"))
  :hook (ruby-mode . flycheck-mode)
  :config
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (setq evil-shift-width ruby-indent-level)))
  :custom
  (ruby-insert-encoding-magic-comment nil)
  (ruby-align-to-stmt-keywords
   '(if while unless until begin case for def)))
(use-package bundler
  :hook (ruby-mode . bundler-mode))
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter-hook . inf-ruby-auto-enter))
  :custom
  (inf-ruby-console-environment "development"))
(use-package company-inf-ruby
  :after inf-ruby
  :config
  (add-to-list 'company-backends 'company-inf-ruby))
(use-package rspec-mode
  :hook (ruby-mode . rspec-mode)
  :custom
  (compilation-scroll-output 'first-error)
  (rspec-autosave-buffer t)
  :config
  (add-hook 'rspec-compilation-mode-hook 'inf-ruby-auto-enter nil t)
  (with-eval-after-load 'smartparens
    (sp-with-modes 'ruby-mode
      (sp-local-pair
       "{" "}"
       :pre-handlers '(sp-ruby-pre-handler)
       :post-handlers '(sp-ruby-post-handler
                        (js|smartparens-pair-newline-and-indent "RET"))
       :suffix ""))))
(use-package rubocop
  :ensure-system-package
  (rubocop . "gem install rubocop")
  :hook (ruby-mode . rubocop-mode))
(use-package rbenv
  :hook (ruby-mode . global-rbenv-mode))
(use-package yard-mode
  :hook (ruby-mode . yard-mode))
(use-package ruby-hash-syntax
  :requires ruby-mode)
(use-package projectile-rails
  :requires projectile
  :hook (projectile-mode . projectile-rails-on))

;; SQL
(use-package sql
  :ensure nil
  :mode "\\.sql$"
  :custom
  (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  (sql-set-product-feature 'postgres :prompt-cont-regexp
                           "^[-[:alnum:]_]*[-(][#>] ")
  :config
  (progn
    (defun my-sql-login-hook ()
      "Custom SQL log-in behaviours. See `sql-login-hook'."
      ;; n.b. If you are looking for a response and need to parse the
      ;; response, use `sql-redirect-value' instead of `comint-send-string'.
      (when (eq sql-product 'postgres)
        (let ((proc (get-buffer-process (current-buffer))))
          ;; Output each query before executing it. (n.b. this also avoids
          ;; the psql prompt breaking the alignment of query results.)
          (comint-send-string proc "\\set ECHO queries\n"))))
    (add-hook 'sql-login-hook 'my-sql-login-hook)
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (toggle-truncate-lines t)))))
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))
(use-package sqlup-mode
  :hook (sql-mode . sql-interactive-mode-hook))

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 2
        lua-indent-string-contents t))
(use-package company-lua
  :hook lua-mode
  :config
  (add-to-list 'company-backends 'company-lua))

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
(use-package vlf
  :hook csv-mode)

;; JSON
(use-package json-mode
  :defer t
  :config
  (setq js-indent-level 2))
(use-package json-snatcher
  :hook json-mode
  :config
  (js|keymap-for-mode 'json-mode
                      "hp" 'jsons-print-path))
(use-package json-reformat
  :hook json-mode
  :commands (spacemacs/json-reformat-code)
  :config
  (defun spacemacs/json-reformat-dwim (arg &optional start end)
    "Reformat the whole buffer of the active region.
If ARG is non-nil (universal prefix argument) then try to decode the strings.
If ARG is a numerical prefix argument then specify the indentation level."
    (interactive "P\nr")
    (let ((json-reformat:indent-width js-indent-level)
          (json-reformat:pretty-string? nil))
      (cond
       ((numberp arg) (setq json-reformat:indent-width arg))
       (arg (setq json-reformat:pretty-string? t)))
      (if (equal start end)
          (save-excursion (json-reformat-region (point-min) (point-max)))
        (json-reformat-region start end)))))


(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")


(use-package yaml-mode
  :mode "\\.ya?ml\'")


(use-package markdown-mode
  :mode "\\.md$"
  :hook (markdown-mode . flyspell-mode))

(use-package langtool
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config
  (unless langtool-language-tool-jar
    (setq langtool-language-tool-jar
          (cond ((eq system-type 'darwin)
                 (locate-file "libexec/languagetool-commandline.jar"
                              (js|files-in "/usr/local/cellar/languagetool"
                                           :type 'dirs
                                           :depth 1)))
                ((eq system-type 'linux)
                 "/usr/share/java/languagetool/languagetool-commandline.jar"))
          langtool-mother-tongue "en-US")))

;; Common Lisp
(use-package sly
  :requires (evil company)
  :hook ((lisp-mode emacs-lisp-mode) . (lambda ()  (sly-setup '(sly-fancy))))
  :defer t
  :custom
  (inferior-lisp-program "sbcl")
  (sly-autodoc-use-multiline t)
  (sly-complete-symbol*-fancy t)
  (sly-kill-without-query-p t)
  (sly-repl-history-remove-duplicates t)
  (sly-repl-history-trim-whitespaces t)
  (sly-net-coding-system 'utf-8-unix)
  :config
  (progn
    (add-to-list 'company-backends 'company-capf)
    ;; (add-to-list 'evil-emacs-state-modes 'sly-mrepl-mode) (this one we want evil)
    (add-to-list 'evil-emacs-state-modes 'sly-inspector-mode)
    (add-to-list 'evil-emacs-state-modes 'sly-db-mode)
    (add-to-list 'evil-emacs-state-modes 'sly-xref-mode)
    (add-to-list 'evil-emacs-state-modes 'sly-stickers--replay-mode)
    (defun +common-lisp|cleanup-sly-maybe ()
      "Kill processes and leftover buffers when killing the last sly buffer."
      (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                       if (and (buffer-local-value 'sly-mode buf)
                               (get-buffer-window buf))
                       return t)
        (dolist (conn (sly--purge-connections))
          (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
        (let (kill-buffer-hook kill-buffer-query-functions)
          (mapc #'kill-buffer
                (cl-loop for buf in (delq (current-buffer) (buffer-list))
                         if (buffer-local-value 'sly-mode buf)
                         collect buf)))))

    (defun +common-lisp|init-sly ()
      "Attempt to auto-start sly when opening a lisp buffer."
      (cond ((sly-connected-p))
            ((executable-find inferior-lisp-program)
             (let ((sly-auto-start 'always))
               (sly-auto-start)
               (add-hook 'kill-buffer-hook #'+common-lisp|cleanup-sly-maybe nil t)))
            ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                      inferior-lisp-program))))
    (add-hook 'sly-mode-hook #'+common-lisp|init-sly)

    (defun +common-lisp*refresh-sly-version (version conn)
      "Update `sly-protocol-version', which will likely be incorrect or nil due to
an issue where `load-file-name' is incorrect. Because Doom's packages are
installed through an external script (bin/doom), `load-file-name' is set to
bin/doom while packages at compile-time (not a runtime though)."
      (unless sly-protocol-version
        (setq sly-protocol-version (sly-version nil (locate-library "sly.el"))))
      (advice-remove #'sly-check-version #'+common-lisp*refresh-sly-version))
    (advice-add #'sly-check-version :before #'+common-lisp*refresh-sly-version)))
(use-package sly-mrepl
  :ensure nil ;; built-in to sly
  :defines sly-mrepl-mode-map
  :bind
  (:map sly-mrepl-mode-map
        ("<up>" . sly-mrepl-previous-input-or-button)
        ("<down>" . sly-mrepl-next-input-or-button)
        ("<C-up>" . sly-mrepl-previous-input-or-button)
        ("<C-down>" . sly-mrepl-next-input-or-button))
  :config
  (with-eval-after-load 'smartparens
    (sp-with-modes '(sly-mrepl-mode)
      (sp-local-pair "'" "'" :actions nil)
      (sp-local-pair "`" "`" :actions nil))))
(use-package sly-repl-ansi-color
  :requires sly
  :demand t
  :config (push 'sly-repl-ansi-color sly-contribs))
;; (use-package sly-company
;; 	:requires (company sly))
;; (use-package slime
;; 	:hook lisp-mode
;; 	:defer t
;; 	:custom
;; 	(inferior-lisp-program "sbcl")
;; 	:config
;; 	(require 'slime-fuzzy)
;; 	(slime-setup)
;; 	:general
;; 	(space-leader-def 'normal lisp-mode
;;     "m '" 'slime

;;     "m c" '(:ignore t :which-key "compile")
;;     "m cc" 'slime-compile-file
;;     "m cC" 'slime-compile-and-load-file
;;     "m cl" 'slime-load-file
;;     "m cf" 'slime-compile-defun
;;     "m cr" 'slime-compile-region
;;     "m cn" 'slime-remove-notes

;;     "m e" '(:ignore t :which-key "eval")
;;     "m eb"  'slime-eval-buffer
;;     "m ef"  'slime-eval-defun
;;     "m eF"  'slime-undefine-function
;;     "m ee"  'slime-eval-last-expression
;;     "m er"  'slime-eval-region

;;     "m g" '(:ignore t :which-key "nav")
;;     "m gb"  'slime-pop-find-definition-stack
;;     "m gn"  'slime-next-note
;;     "m gN"  'slime-previous-note

;;     "m h" '(:ignore t :which-key "help")
;;     "m ha"  'slime-apropos
;;     "m hA"  'slime-apropos-all
;;     "m hd"  'slime-disassemble-symbol
;;     "m hh"  'slime-describe-symbol
;;     "m hH"  'slime-hyperspec-lookup
;;     "m hi"  'slime-inspect-definition
;;     "m hp"  'slime-apropos-package
;;     "m ht"  'slime-toggle-trace-fdefinition
;;     "m hT"  'slime-untrace-all
;;     "m h<"  'slime-who-calls
;;     "m h>"  'slime-calls-who
;;     ;; TODO: Add key bindings for who binds/sets globals?
;;     "m hr"  'slime-who-references
;;     "m hm"  'slime-who-macroexpands
;;     "m hs"  'slime-who-specializes

;;     "m m" '(:ignore t :which-key "macro")
;;     "m ma"  'slime-macroexpand-all
;;     "m mo"  'slime-macroexpand-1

;;     "m s" '(:ignore t :which-key "repl")
;;     "m se"  'slime-eval-last-expression-in-repl
;;     "m si"  'slime
;;     "m sq"  'slime-quit-lisp

;;     "m t" '(:ignore t :which-key "toggle")
;; 		"m tf"  'slime-toggle-fancy-trace
;; 		)
;; 	)
;; (use-package slime-company
;; 	:requires (slime company))
;; (use-package auto-compile
;; 	:commands auto-compile-on-save-mode
;;   :custom
;;   (auto-compile-display-buffer nil)
;; 	(auto-compile-use-mode-line nil))
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode)
  :commands highlight-quoted-mode)
;; (use-package macrostep
;; 	:commands macrostep-expand
;;   ;; :config
;;   ;; (map! :map macrostep-keymap
;;   ;;       :n "RET"    #'macrostep-expand
;;   ;;       :n "e"      #'macrostep-expand
;;   ;;       :n "u"      #'macrostep-collapse
;;   ;;       :n "c"      #'macrostep-collapse

;;   ;;       :n "TAB"    #'macrostep-next-macro
;;   ;;       :n "n"      #'macrostep-next-macro
;;   ;;       :n "J"      #'macrostep-next-macro

;;   ;;       :n "S-TAB"  #'macrostep-prev-macro
;;   ;;       :n "K"      #'macrostep-prev-macro
;;   ;;       :n "p"      #'macrostep-prev-macro

;;   ;;       :n "q"      #'macrostep-collapse-all
;;   ;;       :n "C"      #'macrostep-collapse-all)
;;   ;; ;; `evil-normalize-keymaps' seems to be required for macrostep or it won't
;;   ;; ;; apply for the very first invocation
;; 	;; (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps)
;; 	)
;; (use-package overseer
;; 	:commands overseer-test)

;; Python
(use-package python-mode
  :mode "\\.py")
(use-package anaconda-mode
  :hook python-mode)
(use-package pyenv-mode
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions)
  :hook python-mode)

;; C (via irony-mode)
(use-package ccls
  :requires lsp-mode
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable "~/.local/bin/ccls"))
;; (use-package irony
;;   :hook ((c-mode . irony-mode)
;;          (c++-mode . irony-mode))
;;   :config
;;   (progn
;;     (setq irony-additional-clang-options '("-std=c++11"))
;;     (setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
;;                                                     iron-cdb-libclang))

;;     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;;   (with-eval-after-load 'smartparens
;;     (sp-with-modes '(c++-mode objc-mode)
;;       (sp-local-pair "<" ">"
;;                      :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
;;                      :post-handlers '(("| " "SPC"))))
;;     (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
;;       (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))))
;; (use-package irony-eldoc
;;   :hook (irony-mode . irony-eldoc))
;; (use-package flycheck-irony
;;   :hook (irony-mode . flycheck-irony-setup))
;; (use-package lsp-clangd
;;   :load-path "/vendor"
;;   :hook ((c-mode . lsp-clangd-c-enable)
;;          (c++-mode . lsp-clangd-c++-enable)
;;          (objc-mode . lsp-clangd-objc-enable)))
(use-package platformio-mode
  :hook ((c-mode . platformio-conditionally-enable)
         (c++-mode . platformio-conditionally-enable)))
(use-package clang-format
  :disabled
  :after irony
  :config
  (progn
    (defun c-mode-before-save-hook ()
      (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
        (call-interactively 'clang-format)))

    (add-hook 'before-save-hook #'c-mode-before-save-hook)))
(use-package arduino-mode
  :disabled
  :after irony
  :config
  (add-to-list 'irony-supported-major-modes 'arduino-mode)
  (add-to-list 'irony-lang-compile-option-alist '(arduino-mode . "c++")))

;; Erlang / Elixir
(use-package erlang
  :mode "\\.erl$")
(use-package elixir-mode
  :mode "\\.exs?"
  :config
  (progn
    (defun spacemacs//elixir-enable-compilation-checking ()
      "Enable compile checking if `elixir-enable-compilation-checking' is non nil."
      (when (or elixir-enable-compilation-checking)
        (flycheck-mix-setup)
        ;; enable credo only if there are no compilation errors
        (flycheck-add-next-checker 'elixir-mix '(warning . elixir-credo))))

    (defun spacemacs//elixir-point-after-fn-p (id action context)
      (save-excursion
        (when (looking-back id) (backward-char))
        (looking-back "fn")))

    (defun spacemacs//elixir-looking-back-special-p (expr)
      (save-excursion
        (when (or (looking-back " ")
                  (looking-back "-")) (backward-char))
        (looking-back expr)))

    (defun spacemacs//elixir-do-end-close-action (id action context)
      (when (eq action 'insert)
        (cond ((spacemacs//elixir-looking-back-special-p id)
               (insert " ") (backward-char))
              ((looking-back "(")
               (insert ") ") (backward-char) (backward-char))
              (t
               (newline-and-indent)
               (forward-line -1)
               (indent-according-to-mode)))))
    (with-eval-after-load 'smartparens
      ;; (sp-with-modes 'elixir-mode
      ;;   (sp-local-pair "do" "end"
      ;;                  :when '(("RET" "<evil-ret>"))
      ;;                  :unless '(sp-in-comment-p sp-in-string-p)
      ;;                  :post-handlers '("||\n[i]"))
      ;;   (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
      ;;   (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))
      (sp-with-modes '(elixir-mode)
        (sp-local-pair
         "(" ")"
         :unless '(:add spacemacs//elixir-point-after-fn-p))
        (sp-local-pair
         "fn" "end"
         :when '(("SPC" "RET" "-" "("))
         :post-handlers '(:add spacemacs//elixir-do-end-close-action)
         :actions '(insert))
        (sp-local-pair
         "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(:add spacemacs//elixir-do-end-close-action)
         :actions '(insert))))))
(use-package alchemist
  :disabled
  :hook (elixir-mode . alchemist-mode)
  :config
  (setq alchemist-project-compile-when-needed t
        alchemist-test-status-modeline nil)
  (dolist (mode (list alchemist-compile-mode-map
                      alchemist-eval-mode-map
                      alchemist-execute-mode-map
                      alchemist-message-mode-map
                      alchemist-help-minor-mode-map
                      alchemist-mix-mode-map
                      alchemist-macroexpand-mode-map
                      alchemist-refcard-mode-map
                      alchemist-test-report-mode-map))
    (evil-define-key 'normal mode
      (kbd "q") 'quit-window)))
(use-package flycheck-mix
  :commands (flycheck-mix-setup)
  :init
  (progn
    (add-to-list 'safe-local-variable-values
                 (cons 'elixir-enable-compilation-checking nil))
    (add-to-list 'safe-local-variable-values
                 (cons 'elixir-enable-compilation-checking t))
    (add-hook 'elixir-mode-local-vars-hook
              'spacemacs//elixir-enable-compilation-checking)))

(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode))

(use-package ensime
  :hook (scala-mode . ensime-mode))

(use-package sbt-mode
  :hook (scala-mode . sbt-mode))

(use-package add-node-modules-path
  :hook ((js-mode json-mode typescript-mode) . add-node-modules-path))

(use-package js2-mode
  :mode "\\.m?js\\'"
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :config
  (setq-default js-switch-indent-offset 2
                js-indent-level 2)
  (setenv "NODE_NO_READLINE" "1"))

(use-package typescript-mode
  ;; :mode "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2
        typescript-expr-indent-offset 2))

(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  ;; :bind
  ;; (:map web-mode-map
  ;;       ("," . self-with-space)
  ;;       ("<C-return>" . html-newline-dwim))
  :config
  (setq   web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-enable-auto-quoting nil
          web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook #'turn-off-smartparens-mode))

;; (use-package company-web
;;   :hook web-mode
;;   :config
;;   (add-to-list 'company-backends 'company-web-html))

(use-package css-mode
  :mode "\\.css\\.erb\\'"
  :custom
  (css-indent-offset 2)
  :config
  (add-to-list 'company-backends 'company-css))

(use-package scss-mode
  :mode "\\.scss$")

(use-package ssass-mode
  :mode "\\.sass$")

(use-package web-beautify
  :hook web-mode)

(with-eval-after-load 'smartparens
  (sp-with-modes '(css-mode scss-mode less-css-mode stylus-mode)
    (sp-local-pair "/*" "*/"
                   :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))))

;; Syntax Checking - Flycheck
(use-package flycheck
  :commands (flycheck-=list-errors flycheck-buffer)
  :quelpa (flycheck :fetcher github :repo "flycheck/flycheck")
  :init (global-flycheck-mode)
  :config
  (setq flycheck-rubocop-lint-only t
        flycheck-idle-change-delay 1.75
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-disabled-checkers '(ruby-rubylint))
  (global-flycheck-mode +1))
(use-package flycheck-posframe
  :commands flycheck-posframe-show-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))

(use-package flyspell
  :commands (flyspell-buffer
             flyspell-goto-next-error)
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (not (eq system-type 'windows-nt))
  :commands flyspell-mode
  :hook
  ((text-mode writeroom-mode org-mode markdown-mode gfm-mode) . turn-on-flyspell)
  ;; (prog-mode . flyspell-prog-mode)
  :delight
  :config
  (setq flyspell-issue-message-flag nil
        ;; ispell-silently-savep t
        ispell-program-name (executable-find "aspell")
        ispell-list-command "--list"
        ispell-extra-args '("--sug-mode=ultra"
                            "--lang=en_US"
                            "--dont-tex-check-comments")))

(use-package writeroom-mode
  :commands writeroom-mode)

(use-package all-the-icons)

(use-package base16-theme
  :defer t
  :init
  (load-theme 'base16-oceanicnext t))

(use-package hide-mode-line
  :hook ((neotree-mode
          completion-list-mode
          completion-in-region-mode) . hide-mode-line-mode))

;;; Support Emojis in Emacs
(use-package emojify
  :defer 5
  :custom
  (emojify-display-style 'unicode)
  :hook
  ((markdown-mode
    git-commit-mode
    magit-status-mode
    magit-log-mode) . emojify-mode))

;;; Restart Emacs
(use-package restart-emacs
  :commands restart-emacs)

(use-package winum
  :commands (winum-select-window-by-number
             winum-select-window-0-or-10
             winum-select-window-1
             winum-select-window-2
             winum-select-window-3
             winum-select-window-4
             winum-select-window-5
             winum-select-window-6
             winum-select-window-7
             winum-select-window-8
             winum-select-window-9)
  :config
  (progn
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line nil
          winum-keymap nil
          winum-ignored-buffers '(" *which-key*"))
    (defun winum-assign-0-to-neotree ()
      (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
    (add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
    (winum-mode)))


(use-package window
  :ensure nil
  :preface (provide 'window)
  :custom
  (display-buffer-alist
   `((,(rx bos (or "*Flycheck errors*"
                   "*Backtrace"
                   "*Warnings"
                   "*compilation"
                   "*Help"
                   "*helpful"
                   "*ivy-occur"
                   "*less-css-compilation"
                   "*Packages"
                   "*SQL"))
      (display-buffer-reuse-window
       display-buffer-in-side-window)
      (side            . bottom)
      (reusable-frames . visible)
      (window-height   . 0.5))
     ("." nil (reusable-frames . visible)))))

(use-package files
  :no-require t
  :ensure nil
  :demand t
  :config
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq kept-old-versions 2
        kept-new-versions 6
        backup-by-copying t
        require-final-newline t
        delete-old-versions t
        version-control t
        large-file-warning-threshold (* 20 1000 1000)))

(use-package vc-hooks
  :no-require t
  :ensure nil
  :demand t
  :custom (vc-follow-symlinks t))

(use-package dired
  :no-require t
  :ensure nil
  :demand t
  :commands (dired)
  :config
  (setq dired-dwim-target t ; "Enable side-by-side `dired` buffer targets."
        dired-recursive-copies 'always ; "Better recursion in `dired`."
        dired-recursive-deletes 'top
        delete-by-moving-to-trash t
        dired-use-ls-dired nil))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package uniquify
  :no-require t
  :ensure nil
  :demand t
  :custom (uniquify-buffer-name-style 'forward))

(use-package sh-mode
  :ensure nil
  :mode
  (("\\.zshrc" . sh-mode)
   ("bashrc$" . sh-mode)
   ("bash_profile$" . sh-mode)
   ("bash_aliases$" . sh-mode)
   ("bash_local$" . sh-mode)
   ("bash_completion$" . sh-mode)))

(use-package recentf
  :requires no-littering
  :defer t
  :ensure nil
  :config
  (setq recentf-auto-cleanup 200
        recentf-max-saved-items 300
        recentf-auto-cleanup 'never
        recentf-filename-handlers '(file-truename abbreviate-file-name)
        recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list)
        recentf-exclude (list #'file-remote-p "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
                              "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                              "^/var/folders/.+$" "\\.git/config" "\\.git/COMMIT_EDITMSG"))
  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package eldoc
  :ensure nil
  :delight
  :hook ((ielm-mode eval-expression-minibuffer-setup) . eldoc-mode))

(use-package eshell
  :commands (eshell eshell-mode)
  :config
  (setq eshell-visual-commands '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim")
        eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))
        eshell-history-size 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-output 'this
        eshell-scroll-to-bottom-on-input 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t))

(use-package helpful
  ;; :after ivy
  :commands (helpful-callable
             helpful-command
             helpful-variable
             helpful-key
             helpful-macro
             helpful-function)
  :defer t
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point)))


(use-package org
  :requires langtool
  :defer 3
  :pin org
  :mode "\\.org\'"
  :config
  (progn
    (add-hook 'before-save-hook 'langtool-check)
    (setq org-src-tab-acts-natively t
          org-src-fontify-natively t
          org-return-follows-link t
          org-startup-indented t
          org-insert-heading-respect-content t
          org-hide-leading-stars t
          org-directory "~/org"
          org-M-RET-may-split-line '((item . nil))
          org-default-notes-file (expand-file-name "notes.org" org-directory))
    (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                              (sequence "⚑ WAITING(w)" "|")
                              (sequence "|" "✘ CANCELED(c)")))))
(use-package ox-pandoc
  :after org)
(use-package ox-minutes
  :after org)
(use-package ox-gfm
  :after org)
(use-package ox-asciidoc
  :after org)
(use-package toc-org
  :custom (toc-org-max-depth 10)
  :hook (org-mode . toc-org-enable))
(use-package org-projectile
  :commands org-projectile-projectile-project-todo-completing-read
  :hook (projectile-before-switch-project-hook . org-projectile-per-project)
  :config
  (setq org-projectile-per-project-filepath "TODO.org"
        setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))
(use-package org-bullets
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))
(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :custom
  (evil-org-use-additional-insert t)
  (evil-org-key-theme '(textobjects
                        navigation
                        additional
                        todo)))

(use-package pdf-tools
  :disabled
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package matrix-client
  :disabled ;; not ready for prime time yet
  :quelpa (matrix-client :fetcher github
                         :repo "jgkamat/matrix-client-el"))

(use-package linux
  :ensure nil
  :load-path "vendor/"
  :if (eq system-type 'gnu/linux))

(use-package osx
  :ensure nil
  :load-path "vendor/"
  :if (eq system-type 'darwin))

(require '+completion)
(require 'deprecate)
(require '+keybindings)

(customize-set-variable 'user-full-name "Justin Smestad")
(customize-set-variable 'user-mail-address "justin.smestad@gmail.com")


(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 2
              indent-tabs-mode nil)

(setq fill-column 125 ; Use Github as the standard, ref http://hilton.org.uk/blog/source-code-line-length
      inhibit-startup-screen t
      blink-matching-paren nil
      visible-bell nil
      ring-bell-function 'ignore

      ;; silence ad-handle-definition about advised functions getting redefined
      ad-redefinition-action 'accept

      ;; Window Tweaks
      window-resize-pixelwise t
      frame-resize-pixelwise t

      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      idle-update-delay 2 ; update ui less often (0.5 default)
      create-lockfiles nil
      cua-mode t
      desktop-save-mode nil
      indent-tabs-mode nil
      initial-scratch-message nil

      load-prefer-newer t ; Prevent loading old code

      sentence-end-double-space nil
      ansi-color-for-comint-mode t
      cusor-in-non-selected-windows nil ; hide cursors in other windows
      display-line-numbers-width 3
      enable-recursive-minibuffers nil

      ;; `pos-tip' defaults
      pos-tip-internal-border-width 6
      pos-tip-border-width 1

      inhibit-compacting-font-caches t
      find-file-visit-truename t

      history-delete-duplicates t ; Get rid of duplicates in minibuffer history
      ;; keep the point out of the minibuffer
      minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; Adjust Font Faces for Company
(custom-set-faces
 '(company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

;; relegate tooltips to echo area only
(if (boundp 'tooltip-mode) (tooltip-mode -1))

;; Handle ansi codes in compilation buffer
(defun doom|apply-ansi-color-to-compilation-buffer ()
  "Apply ansi codes to the compilation buffers. Meant for 'compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'doom|apply-ansi-color-to-compilation-buffer)

;; This is MUCH faster than using set-face-attribute
(add-to-list 'default-frame-alist '(font . "Fira Code:13"))

;; Appearance
;; Theme Emacs for dark color scheme
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(provide 'config)
;;; config.el ends here
