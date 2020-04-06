;;; config.el --- Justin's Emacs config -*- lexical-binding: t; -*-
;;; Commentary:

;;; TODO dump-jump after xref (instead of dump jump first)
;;; TODO evil-lion
;;; TODO try outshine and bicycle for hide/show in prog mode
;;; TODO try dired-sidebar instead of neotree/treemacs
;;; TODO take over window-purpose from spacemacs
;;; TODO popups need to be controlled to use the same area of screen
;;; TODO word wrap in completion buffers by default
;;; TODO no line numbers in completion buffers
;;; TODO look into emacs-quickrun (run commands quickly)

;;; Code:

(use-package centaur-tabs
  :commands (centaur-tabs-mode)
  :defines (centaur-tabs-set-icons
            centaur-tabs-style
            centaur-tabs-set-bar
            centaur-tabs-set-modified-marker
            centaur-tabs-height)
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-style "bar"
        centaur-tabs-set-bar 'left
	      centaur-tabs-set-modified-marker t
        centaur-tabs-height 32)
  (centaur-tabs-change-fonts "Ubuntu" 120)
  (with-eval-after-load 'projectile
    (centaur-tabs-group-by-projectile-project))
  :bind
  (:map evil-normal-state-map
	      ("g t" . centaur-tabs-forward)
	      ("g T" . centaur-tabs-backward)))

(defvar +completion-engine 'ivy
  "Setting to control whether to use helm or ivy.")
(if (eq +completion-engine 'helm)
    (require '+helm)
  (require '+ivy))

(if (eq system-type 'darwin)
    (require 'osx)
  (require 'linux))

(require 'web)
(require 'ember)
(require 'ruby)
(require 'elixir)
(require 'golang)
(require 'write)

(require '+company)
(require '+org)
(require '+dired)
(require 'deprecate)
(require '+keybindings)

(use-package rg)

(use-package vimish-fold)
(use-package evil-escape
  :disabled
  :load-path "vendor/" ; Vendored due to missing vterm support - https://github.com/syl20bnr/evil-escape/pull/87
  :diminish
  :config
  (setq evil-escape-delay 0.2
        evil-escape-excluded-major-modes '(vterm-mode)
        evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package evil-surround
  :diminish
  :hook (after-init . global-evil-surround-mode))
(use-package evil-matchit
  :diminish
  :hook (prog-mode . turn-on-evil-matchit-mode))
(use-package evil-goggles
  :defer t
  :diminish
  :config
  ;;  (setq evil-goggles-duration 0.1
  ;;        evil-goggles-enable-delete nil)
  (evil-goggles-mode))
(use-package evil-commentary
  :diminish
  :hook (after-init . evil-commentary-mode))
(use-package evil-collection
  :init
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))
(use-package evil-expat
  :defer t)

(use-package auto-sudoedit
  :hook (after-init . auto-sudoedit-mode))

;; EditorConfig
(use-package editorconfig
  :hook (prog-mode . editorconfig-mode)
  :config
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

(use-package direnv
  :config
  (add-to-list 'direnv-non-file-modes 'vterm-mode)
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package perspective
  ;; :hook (persp-switch . my/persp-neo)
  :config
  (setq persp-initial-frame-name "none"
        persp-show-modestring nil)
  (defhydra perspective-hydra (:color blue)
    "
^
^Perspective^         ^Do^                ^Switch^
^─────────^─────────^──^────────────────^──────^────────────
_q_ quit            ^^                  _<_ previous
^^                  _k_ kill            _>_ next
^^                  _r_ rename          _b_ switch to buffer
^^                  _a_ add buffer      _s_ switch
^^                  _A_ set buffer      _p_ switch project
"
    ("q" nil)
    ("a" persp-add-buffer)
    ("A" persp-set-buffer)
    ("<" persp-prev :color red)
    (">" persp-next :color red)
    ;; ("c" eyebrowse-create-window-config)
    ("b" persp-switch-to-buffer)
    ("p" projectile-persp-switch-buffer)
    ("k" persp-kill :color red)
    ("r" persp-rename)
    ("s" persp-switch))
  (defun my/persp-neo ()
    "Make NeoTree follow the perspective"
    (interactive)
    (let ((cw (selected-window))
          (path (buffer-file-name))) ;;save current window/buffer
      (progn
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p)
                   (fboundp 'projectile-project-root))
          (neotree-dir (projectile-project-root)))
        (neotree-find path))
      (select-window cw)))

  (persp-mode))
(use-package persp-projectile
  :after perspective)
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
  (add-hook 'eyebrowse-post-switch-hook #'neo-global-attach)
  (eyebrowse-mode t))

(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-indexing-method 'alien
        ;; projectile-enable-caching nil
        projectile-use-git-grep t
        projectile-sort-order 'recentf
        projectile-files-cache-expire 604800 ; expire after a week
        projectile-switch-project-action 'counsel-projectile-find-file
        projectile-globally-ignored-files (append '(".git" ".DS_Store" "Icon" "TAGS") projectile-globally-ignored-directories)
        projectile-globally-ignored-file-suffixes (append '("*.jar" "*.elc" "*.pyc" "*.o") projectile-globally-ignored-files))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-project-root-files ".clang_complete"))


(use-package amx
  :hook (after-init . amx-initialize))

(use-package zoom
  :commands zoom-mode)



;; Language Server Protocol (LSP)
(use-package lsp-mode
  :diminish lsp-mode
  :hook
  (prog-mode . (lambda ()
                 (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                   (lsp-deferred))))
  :config
  (require 'lsp-clients)
  (setq lsp-auto-guess-root t
        ;; lsp-enable-snippet t
        lsp-prefer-flymake nil
        ;; lsp-enable-indentation t ;; TODO is this messing emacs?
        lsp-enable-xref t
        lsp-enable-imenu t
        ;; lsp-prefer-capf t
        ;; lsp-enable-completion-at-point t
        ;; lsp-before-save-edits t
        lsp-solargraph-formatting t
        lsp-solargraph-diagnostics nil
        lsp-solargraph-rename nil
        lsp-keep-workspace-alive nil)
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection (list "node" (concat (getenv "HOME") "/code/github/ember-language-server/lib/start-server.js") "--stdio"))
  ;;                   :major-modes '(js-mode js2-mode)
  ;;                   :priority -1
  ;;                   :ignore-messages (list "Initializing Ember Language Server at .*$")
  ;;                   :add-on? t
  ;;                   :server-id 'ember-ls))
  (add-to-list 'exec-path "~/code/github/elixir-ls/release"))

(use-package lsp-ui
  ;; :disabled
  :hook (elixir-mode . lsp-ui-doc-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-delay 3
        lsp-ui-doc-include-signature t
        lsp-ui-doc-header t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-use-webkit nil ;; It is ugly and too big
        ;; lsp-ui-doc-border (face-foreground 'default)

        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package dap-mode
  :diminish
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (dap-ui-mode . dap-tooltip-mode)

         (dap-session-created . (lambda (_args) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))

         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
         (elixir-mode . (lambda () (require 'dap-elixir)))))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dtrt-indent
  :diminish
  :config (setq dtrt-indent-min-quality 60)
  :hook (after-init . dtrt-indent-global-mode))

(use-package indent-guide
  :diminish
  :commands (indent-guide-mode indent-guide-global-mode)
  :config (setq indent-guide-delay 0.3))

(use-package aggressive-indent
  :hook (((emacs-lisp-mode css-mode) . aggressive-indent-mode)))

(use-package dumb-jump
  :config
  (setq dump-jump-prefer-searcher 'rg
        dumb-jump-selector +completion-engine)
  (defhydra hydra-dumb-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode)
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :diminish
  :config
  (setq auto-revert-verbose nil
        auto-revert-check-vc-info t))

(use-package undo-tree
  :delight
  :config
  (setq undo-tree-auto-save-history nil)
  :hook (after-init . global-undo-tree-mode))

(use-package unfill
  :disabled
  :bind ([remap fill-paragraph] . #'unfill-toggle))

(use-package hl-todo
  :defer t
  :init (global-hl-todo-mode))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :custom (highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package highlight-indentation
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode))

(use-package visual-fill-column
  :hook ((text-mode org-mode) . visual-fill-column-mode)
  :config
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
  (advice-add 'text-scale-adjust :after
              #'visual-fill-column-adjust))

(use-package swiper
  :after evil
  :config
  (setq swiper-action-recenter t)
  :general
  (general-define-key
   "C-s" 'swiper))

(use-package midnight)


;; SQL
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
  :mode "\\.csv\\'"
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
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))
(use-package json-snatcher
  :commands (jsons-print-path)
  :config
  (js|keymap-for-mode 'json-mode
                      "hp" 'jsons-print-path))

(use-package dockerfile-mode
  :mode "\\Dockerfile.*\\'")
(use-package docker ;; TODO finish customizing this
  :config
  (defhydra hydra-docker (:columns 5 :color blue)
    "Docker"
    ("c" docker-containers "Containers")
    ("v" docker-volumes "Volumes")
    ("i" docker-images "Images")
    ("n" docker-networks "Networks")
    ("b" dockerfile-build-buffer "Build Buffer")
    ("q" nil "Quit")))

(use-package yaml-mode
  :mode "\\.ya?ml\'")

;; LaTeX
(with-eval-after-load 'latex-mode
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'rainbow-delimiters-mode)
  (use-package latex-preview-pane)
  (use-package company-auctex))

;; C (via irony-mode)
(use-package ccls
  :requires lsp-mode
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls)))
  :config
  (setq ccls-executable "ccls"))
(use-package platformio-mode
  :hook ((c-mode c++-mode) . platformio-conditionally-enable))
(use-package clang-format
  :commands (clang-format))


(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode))
(use-package sbt-mode
  :hook (scala-mode . sbt-mode))

;; Elm
;; NOTE watch for the release of an LSP for Elm (none as of 2019-05)
(use-package elm-mode
  :mode "\\.elm\\'"
  :config
  (setq elm-tags-on-save t
        elm-format-on-save t)
  (add-to-list 'company-backends 'company-elm))
(use-package elm-test-runner
  :after elm-mode)
(use-package flycheck-elm
  :hook (elm-mode . flycheck-elm-setup))

;; OCaml / ReasonML
(use-package reason-mode
  :mode "\\.rei?\\'"
  :config
  (add-hook 'reason-mode-hook (lambda ()
                                (add-hook 'before-save-hook 'reason/refmt-before-save nil t))))
(use-package merlin
  :after reason-mode
  :config
  (setq merlin-completion-with-doc t))
(use-package flycheck-ocaml
  :after reason-mode)

;; Syntax Checking - Flycheck
(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-global-modes
        '(not org-mode text-mode outline-mode fundamental-mode
              diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-rubocop-lint-only t
        flycheck-idle-change-delay 1.75
        flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq-default flycheck-disabled-checkers '(ruby-rubylint
                                             emacs-lisp-checkdoc))
  ;; Prettify fringe style
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  )
(use-package flycheck-posframe
  :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
  :hook (flycheck-mode . flycheck-posframe-mode)
  :init (setq flycheck-posframe-border-width 1
              flycheck-posframe-inhibit-functions
              '((lambda (&rest _) (bound-and-true-p company-backend)))))
(use-package flycheck-pos-tip
  :defines flycheck-pos-tip-timeout
  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-timeout 30))

(use-package doom-modeline
  :hook (winum-mode . doom-modeline-mode)
  :config
  (setq doom-modeline-indent-info nil
        ;; doom-modeline-display-default-persp-name t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-project-detection 'projectile
        doom-modeline-lsp t
        doom-modeline-env-version nil))

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (declare-function memoize 'memoize)
  (declare-function memoize-restore 'memoize)
  (defun all-the-icons-reset ()
    "Reset (unmemoize/memoize) the icons."
    (interactive)
    (ignore-errors
      (dolist (f '(all-the-icons-icon-for-file
                   all-the-icons-icon-for-mode
                   all-the-icons-icon-for-url
                   all-the-icons-icon-family-for-file
                   all-the-icons-icon-family-for-mode
                   all-the-icons-icon-family))
        (memoize-restore f)
        (memoize f)))
    (message "Reset all-the-icons"))

  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.mod$" all-the-icons-fileicon "go" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.sum$" all-the-icons-fileicon "go" :face all-the-icons-dpurple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "playlist_play" :height 1.2 :v-adjust -0.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :v-adjust -0.2 :face 'all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))


;; Themes
(use-package composite ; Use symbols in fonts (requires Emacs >= 27)
  :if (version<= "27.0" emacs-version)
  :straight nil
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  (let ((alist
         '((?! . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
           (?# . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
           (?$ . ".\\(?:\\(>\\)>?\\)")
           (?% . ".\\(?:\\(%\\)%?\\)")
           (?& . ".\\(?:\\(&\\)&?\\)")
           (?* . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
           ;; (?* . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
           (?+ . ".\\(?:\\([>]\\)>?\\)")
           ;; (?+ . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
           (?- . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
           ;; (?. . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
           (?. . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
           (?/ . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
           ;; (?/ . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
           (?0 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
           (?: . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
           (59 . ".\\(?:\\(;\\);?\\)")    ;; 59 is ;
           (?< . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
           (?= . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
           (?> . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
           (?? . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
           (91 . ".\\(?:\\(|\\)[]|]?\\)") ;; 91 is [
           ;; (?\ . ".\\(?:\\([\\n]\\)[\\]?\\)")
           (?^ . ".\\(?:\\(=\\)=?\\)")
           (?_ . ".\\(?:\\(|_\\|[_]\\)_?\\)")
           (?w . ".\\(?:\\(ww\\)w?\\)")
           (?{ . ".\\(?:\\(|\\)[|}]?\\)")
           (?| . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
           (?~ . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-ligature-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  (set-char-table-parent composition-ligature-table composition-function-table))

(use-package base16-theme)
(use-package doom-themes
  :config
  ;; (setq doom-treemacs-enable-variable-pitch t)
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-nord-light t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-spacegrey t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  (doom-themes-neotree-config)
  (doom-themes-treemacs-config))


;; Hide Mode Line
(use-package hide-mode-line
  :hook ((neotree-mode
          treemacs-mode
          vterm-mode
          completion-list-mode
          completion-in-region-mode) . hide-mode-line-mode))

;;; Support Emojis in Emacs
(use-package emojify
  :diminish
  :config
  (setq emojify-display-style (if (eq system-type 'gnu/linux) 'image 'unicode)
        emojify-company-tooltips-p t)
  :hook
  ((markdown-mode
    git-commit-mode
    magit-status-mode
    magit-log-mode) . emojify-mode))

;; Git
(use-package magit
  :config
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/usr/local/bin/git")))
(use-package evil-magit
  :after magit)

;; Restart Emacs
(use-package restart-emacs
  :commands restart-emacs)

;; Window Numbers
(use-package winum
  :demand
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
  :straight nil
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

(use-package smartparens
  :hook (prog-mode . smartparens-global-mode)
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
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression' or `evil-ex'."
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
    (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)))

(use-package ibuffer
  :straight nil
  :functions (my-ibuffer-find-file)
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (with-eval-after-load 'counsel
    (with-no-warnings
      (defun my-ibuffer-find-file ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory)
                                     default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))))

(use-package all-the-icons-ibuffer
  :init
  (setq all-the-icons-ibuffer-icon-size 0.85)
  (all-the-icons-ibuffer-mode 1))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (if (display-graphic-p)
            (concat
             (all-the-icons-octicon "file-directory"
                                    :face ibuffer-filter-group-name-face
                                    :v-adjust 0.0
                                    :height 1.0)
             " ")
          "Project: ")))
;; Files
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
      large-file-warning-threshold (* 20 1000 1000))

(setq vc-follow-symlinks t)

(use-package display-line-numbers
  :straight nil
  :hook (prog-mode . display-line-numbers-mode))

(setq uniquify-buffer-name-style 'forward)

(use-package sh-mode
  :straight nil
  :mode
  (("\\.zshrc" . sh-mode)
   ("bashrc$" . sh-mode)
   ("bash_profile$" . sh-mode)
   ("bash_aliases$" . sh-mode)
   ("bash_local$" . sh-mode)
   ("bash_completion$" . sh-mode)))

(use-package recentf
  :straight nil
  :requires no-littering
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

(use-package vterm ; https://github.com/akermu/emacs-libvterm
  :commands (vterm vterm-other-window)
  :general
  (general-nmap vterm-mode-map
    [escape] 'vterm--self-insert
    [return] 'vterm--self-insert
    "p" 'vterm-yank
    "u" 'vterm-undo)
  (general-imap vterm-mode-map
    "C-y" 'vterm-yank)
  (general-def vterm-mode-map
    "M-n" 'vterm-send-down
    "M-p" 'vterm-send-up
    "M-y" 'vterm-yank-pop
    "M-/" 'vterm-send-tab)
  :load-path "~/code/github/emacs-libvterm")

(use-package helpful
  :commands (helpful-callable
             helpful-command
             helpful-variable
             helpful-key
             helpful-macro
             helpful-function)
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point)))

(use-package adoc-mode ; asciidoc support
  :commands (tempo-template-adoc-title-1
             tempo-template-adoc-title-2
             tempo-template-adoc-title-3
             tempo-template-adoc-title-4
             tempo-template-adoc-title-5
             tempo-template-adoc-strong
             tempo-template-adoc-emphasis
             adoc-demote
             adoc-promote)
  :mode "\\.adoc?\\'")

;; Language Files for Elixir
(use-package po-mode
  :mode "\\.pot?\\'")


(customize-set-variable 'user-full-name "Justin Smestad")
(customize-set-variable 'user-mail-address "justin.smestad@gmail.com")


(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 2
              indent-tabs-mode nil
              fill-column 125 ; Use Github as the standard, ref http://hilton.org.uk/blog/source-code-line-length
              )

(setq inhibit-startup-screen t
      blink-matching-paren nil
      visible-bell nil
      ring-bell-function 'ignore

      ;; silence ad-handle-definition about advised functions getting redefined
      ad-redefinition-action 'accept

      ;; Window Tweaks
      window-resize-pixelwise t

      ;; Inhibit resizing frame
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t

      find-file-visit-truename t ; resolve symlinks when opening files

      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      idle-update-delay 1 ; update UI slightly less often (0.5 default)
      create-lockfiles nil
      cua-mode t
      desktop-save-mode nil
      indent-tabs-mode nil

      make-backup-files nil ; don't create backup~ files

      initial-scratch-message nil
      initial-major-mode 'org-mode

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

;; Handle ANSI codes in compilation buffer
(add-hook 'compilation-filter-hook #'doom|apply-ansi-color-to-compilation-buffer)

(load "~/.emacs.secrets" t)

(use-package server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'config)
;;; config.el ends here
