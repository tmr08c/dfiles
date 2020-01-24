;;; config.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; TODO try outshine and bicycle for hide/show in prog mode
;;; TODO try dired-sidebar instead of neotree/treemacs
;;;
;;; TODO popups need to be controlled to use the same area of screen
;;; TODO word wrap in completion buffers by default
;;; TODO no line numbers in completion buffers
;;; TODO look into emacs-quickrun (run commands quickly)
;;;
;;; Code:


(require '+funcs)

(defvar +completion-engine 'ivy
  "Setting to control whether to use helm or ivy.")

(use-package exec-path-from-shell
  :diminish
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package which-key
  :defer 1
  :diminish
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package general
  :after evil
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
  (general-evil-setup)
  (general-vmap "," (general-simulate-key "SPC m"))
  (general-nmap "," (general-simulate-key "SPC m")))

(use-package evil
  :demand
  :init (setq evil-want-C-u-scroll t
              evil-want-integration t
              evil-want-keybinding nil) ; This MUST be in init.
  :config
  (setq evil-want-visual-char-semi-exclusive t
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t
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
  (evil-set-initial-state 'Custom-mode 'emacs)

  (defun +evil|update-shift-width ()
    (setq evil-shift-width tab-width))
  (add-hook 'after-change-major-mode-hook #'+evil|update-shift-width t)

  (evil-mode 1))

(use-package evil-escape
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
  :hook (after-init . global-evil-matchit-mode))
(use-package evil-goggles
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

(use-package auto-sudoedit
  :hook (after-init . auto-sudoedit-mode))

;; EditorConfig
(use-package editorconfig
  :hook (prog-mode . editorconfig-mode)
  :config
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

(use-package direnv
  :hook
  ((flycheck-before-syntax-check . direnv-update-environment)
   (before-hack-local-variables . direnv-update-environment)
   (prog-mode . direnv-mode))
  :config
  (setq direnv-always-show-summary nil))

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
  (eyebrowse-mode t))

(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-indexing-method 'alien
        ;; projectile-enable-caching nil
        projectile-use-git-grep t
        projectile-sort-order 'recentf
        projectile-files-cache-expire 604800 ; expire after a week
        ;; projectile-switch-project-action 'projectile-find-file
        projectile-globally-ignored-files (append '(".git" ".DS_Store" "Icon" "TAGS") projectile-globally-ignored-directories)
        projectile-globally-ignored-file-suffixes (append '("*.jar" "*.elc" "*.pyc" "*.o") projectile-globally-ignored-files))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-project-root-files ".clang_complete"))


(use-package amx
  :hook (after-init . amx-initialize))

(use-package zoom
  :commands zoom-mode)

;; Company
(use-package company
  :diminish
  :defines company-backends
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map ; Use tab for completion
              ( "RET" . nil)
              ( [return] . nil )
              ( "tab" . company-complete-selection )
              ( "<tab>" . company-complete-selection ))
  :config
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        ;; company-dabbrev-code-other-buffers t
        company-echo-delay (if (display-graphic-p) nil 0) ; remove annoying blinking
        company-idle-delay 0.6 ; 0.6
        company-minimum-prefix-length 3 ; 3
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-limit 12
        company-global-modes
        '(not eshell-mode shell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        ;; company-transformers '(company-sort-by-occurrence)
        company-backends '(company-capf)))
(use-package company-quickhelp
  :defines company-quickhelp-delay
  :bind (:map company-active-map
              ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :init (setq company-quickhelp-delay 0.5))
(use-package company-emoji
  :requires company
  :config
  (add-to-list 'company-backends 'company-emoji))
(use-package company-prescient
  :init (company-prescient-mode 1))
;; (use-package company-posframe
;;   :hook (company-mode . company-posframe-mode))
(use-package company-box
  :diminish
  :functions (my-company-box--make-line
              my-company-box-icons--elisp)
  :hook (company-mode . company-box-mode)
  :init (setq company-box-backends-colors nil
              company-box-show-single-candidate t
              company-box-max-candidates 50
              company-box-doc-delay 0.5)
  :config
  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (when annotation
                            (concat " " (and company-tooltip-align-annotations
                                             (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                       'company-box--color s-color)
                           line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
          (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
          (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
          (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
          (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
        company-box-icons-alist 'company-box-icons-all-the-icons)
  )

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
        lsp-enable-snippet t
        lsp-prefer-flymake nil
        lsp-enable-indentation t
        lsp-before-save-edits t
        lsp-keep-workspace-alive nil)
  (add-to-list 'exec-path "~/code/github/elixir-ls/release"))
;; Ivy integration
(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
              ("C-s-." . lsp-ivy-global-workspace-symbol)))
(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto)
  :config
  ;; WORKAROUND:Fix tons of unrelated completion candidates shown
  ;; when a candidate is fulfilled
  ;; @see https://github.com/emacs-lsp/lsp-python-ms/issues/79
  (add-to-list 'company-lsp-filter-candidates '(mspyls))

  (with-no-warnings
    (defun my-company-lsp--on-completion (response prefix)
      "Handle completion RESPONSE.
PREFIX is a string of the prefix when the completion is requested.
Return a list of strings as the completion candidates."
      (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
             (items (cond ((hash-table-p response) (gethash "items" response))
                          ((sequencep response) response)))
             (candidates (mapcar (lambda (item)
                                   (company-lsp--make-candidate item prefix))
                                 (lsp--sort-completions items)))
             (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace)))
             (should-filter (or (eq company-lsp-cache-candidates 'auto)
                                (and (null company-lsp-cache-candidates)
                                     (company-lsp--get-config company-lsp-filter-candidates server-id)))))
        (when (null company-lsp--completion-cache)
          (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache nil t)
          (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache nil t))
        (when (eq company-lsp-cache-candidates 'auto)
          ;; Only cache candidates on auto mode. If it's t company caches the
          ;; candidates for us.
          (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
        (if should-filter
            (company-lsp--filter-candidates candidates prefix)
          candidates)))
    (advice-add #'company-lsp--on-completion :override #'my-company-lsp--on-completion)))

(use-package lsp-ui
  :disabled
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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dtrt-indent
  :diminish
  :config (setq dtrt-indent-min-quality 60)
  :hook (after-init . dtrt-indent-global-mode))

(use-package indent-guide
  :commands (indent-guide-mode indent-guide-global-mode)
  :config (setq indent-guide-delay 0.3))

(use-package aggressive-indent
  :hook (((emacs-lisp-mode css-mode) . aggressive-indent-mode)))

(use-package dumb-jump
  :commands (dump-jump-go
             dumb-jump-go-other-window
             dump-jump-go-prompt
             dump-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window)
  :config
  (setq dump-jump-force-searcher 'rg
        dumb-jump-selector +completion-engine))

(use-package ws-butler
  :defer t
  :delight
  :hook (prog-mode . ws-butler-mode)
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

(use-package autorevert
  :ensure nil
  :defer t
  :delight auto-revert-mode
  :config
  (setq auto-revert-verbose nil
        auto-revert-check-vc-info t)
  (global-auto-revert-mode +1))

(use-package undo-tree
  :delight
  :config
  (setq undo-tree-auto-save-history nil)
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
  :commands hide/show-comments-toggle)

(use-package yasnippet
  :disabled
  :hook ((prog-mode snippet-mode) . yas-minor-mode-on)
  :commands
  (yas-minor-mode yas-minor-mode-on yas-expand yas-expand-snippet
                  yas-lookup-snippet yas-insert-snippet yas-new-snippet
                  yas-visit-snippet-file snippet-mode)
  :config
  (setq yas-also-auto-indent-first-line t
        yas-triggers-in-field t) ; Allow nested snippets

  ;; fix an error caused by smartparens interfering with yasnippet bindings
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)

  ;; Exit snippets on ESC from normal mode
  (add-hook '+evil-esc-hook #'yas-exit-all-snippets))
(use-package yasnippet-snippets
  :after yasnippet)

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

(use-package rg
  :defer t
  :commands (rg rg-project rg-dwim rg-literal))

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

;; LaTeX
(with-eval-after-load 'latex-mode
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'rainbow-delimiters-mode))
(use-package latex-preview-pane)
(use-package company-auctex)

;; Python
(use-package python-mode
  :mode "\\.py\\'")
(use-package anaconda-mode
  :hook python-mode)

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
  (setq flycheck-global-modes '(not org-mode text-mode outline-mode fundamental-mode
                                    shell-mode eshell-mode term-mode vterm-mode)
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
  (setq
   doom-modeline-buffer-file-name-style 'relative-from-project
   doom-modeline-project-detection 'projectile
   doom-modeline-lsp t))

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (with-no-warnings
    ;; FIXME: Align the directory icons
    ;; @see https://github.com/domtronn/all-the-icons.el/pull/173
    (defun all-the-icons-icon-for-dir (dir &optional chevron padding)
      "Format an icon for DIR with CHEVRON similar to tree based directories."
      (let* ((matcher (all-the-icons-match-to-alist (file-name-base (directory-file-name dir)) all-the-icons-dir-icon-alist))
             (path (expand-file-name dir))
             (chevron (if chevron (all-the-icons-octicon (format "chevron-%s" chevron) :height 0.8 :v-adjust -0.1) ""))
             (padding (or padding "\t"))
             (icon (cond
                    ((file-symlink-p path)
                     (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.0))
                    ((all-the-icons-dir-is-submodule path)
                     (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.0))
                    ((file-exists-p (format "%s/.git" path))
                     (format "%s" (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.0)))
                    (t (apply (car matcher) (list (cadr matcher) :v-adjust 0.0))))))
        (format "%s%s%s%s%s" padding chevron padding icon padding)))

    (defun all-the-icons-reset ()
      "Reset (unmemoize/memoize) the icons."
      (interactive)
      (dolist (f '(all-the-icons-icon-for-file
                   all-the-icons-icon-for-mode
                   all-the-icons-icon-for-url
                   all-the-icons-icon-family-for-file
                   all-the-icons-icon-family-for-mode
                   all-the-icons-icon-family))
        (ignore-errors
          (memoize-restore f)
          (memoize f)))
      (message "Reset all-the-icons")))

  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
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
	:disabled
  :ensure nil
  :if (version<= "27.0" emacs-version)
  :defer t
  :config
  (dolist (hook `(ediff-mode-hook
                  mu4e-headers-mode-hook
                  package-menu-mode-hook))
    (add-hook hook (lambda () (setq-local auto-composition-mode nil))))

  ;; support ligatures, some toned down to prevent hang
  (let ((alist
         '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
           (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
           (36 . ".\\(?:\\(>\\)>?\\)")
           (37 . ".\\(?:\\(%\\)%?\\)")
           (38 . ".\\(?:\\(&\\)&?\\)")
           (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
           ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
           (43 . ".\\(?:\\([>]\\)>?\\)")
           ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
           (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
           (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
           (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
           ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
           (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
           (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
           (59 . ".\\(?:\\(;\\);?\\)")
           (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
           (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
           (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
           (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
           (91 . ".\\(?:\\(|\\)|?\\)")
           ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
           (94 . ".\\(?:\\(=\\)=?\\)")
           (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
           (119 . ".\\(?:\\(ww\\)w?\\)")
           (123 . ".\\(?:\\(|\\).?\\)")
           (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
           (126 . ".\\(?:\\(~>\\|[-=>@~]\\).?\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))
(use-package base16-theme)
(use-package doom-themes
  :config
  ;; (setq doom-treemacs-enable-variable-pitch t)
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (set-face-attribute 'doom-visual-bell nil
                      :background (face-foreground 'error)
                      :foreground (face-background 'default)
                      :inverse-video nil)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Enable custom treemacs theme (all-the-icons must be installed!)
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
  :defer t
  :config
  (setq emojify-display-style (if (eq system-type 'gnu/linux) 'image 'unicode)
        emojify-company-tooltips-p t)
  :hook
  ((markdown-mode
    git-commit-mode
    magit-status-mode
    magit-log-mode) . emojify-mode))

;; Git
(use-package magit)
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


(use-package ibuffer
  :straight nil
  :ensure nil
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon
              my-ibuffer-find-file)
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; Display buffer icons on GUI
  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    ;; For alignment, the size of the name field should be the width of an icon
    (define-ibuffer-column icon (:name "  ")
      (let ((icon (if (and (buffer-file-name)
                           (all-the-icons-auto-mode-match?))
                      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
          icon)))

    (setq ibuffer-formats `((mark modified read-only locked
                                  ;; Here you may adjust by replacing :right with :center or :left
                                  ;; According to taste, if you want the icon further from the name
                                  " " (icon 2 2 :left :elide)
                                  ,(propertize " " 'display `(space :align-to 8))
                                  (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename))))

  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file)))
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
                                    :v-adjust -0.05
                                    :height 1.25)
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
(setq dired-dwim-target t ; "Enable side-by-side `dired` buffer targets."
        dired-recursive-copies 'always ; "Better recursion in `dired`."
        dired-recursive-deletes 'top
        delete-by-moving-to-trash t
        dired-use-ls-dired nil)

(use-package display-line-numbers
  :straight nil
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(setq uniquify-buffer-name-style 'forward)

(use-package sh-mode
  :straight nil
  :ensure nil
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


(use-package linux
  :straight nil
  :ensure nil
  :load-path "vendor/"
  :if (eq system-type 'gnu/linux))

(use-package osx
  :straight nil
  :ensure nil
  :load-path "vendor/"
  :if (eq system-type 'darwin))

(require 'web)
(require 'ember)
(require 'ruby)
(require 'elixir)
(require 'golang)
(require 'write)

(require '+org)
(require '+dired)
(require '+completion)
(require 'deprecate)
(require '+keybindings)

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
(provide 'config)
;;; config.el ends here
