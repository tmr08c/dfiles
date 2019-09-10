;;; config.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; FIXME evil has issues with Messages buffer
;;; FIXME how to exit snake in evil mode
;;;
;;; TODO popups need to be controlled to use the same area of screen
;;; TODO word wrap in completion buffers by default
;;; TODO no line numbers in completion buffers
;;;
;;; Code:

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 40000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold most-positive-fixnum))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold 800000))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

(require '+funcs)

(defvar +completion-engine 'ivy
  "Setting to control whether to use helm or ivy.")

(use-package dashboard
  :config
  (setq dashboard-set-heading-icons nil
        dashboard-set-file-icons t
        dashboard-startup-banner 'logo
        show-week-agenda-p t

        ;; show Dashboard in frames created with emacsclient -c
        initial-buffer-choice (lambda () (get-buffer "*dashboard*"))

        dashboard-items '((agenda . 5)
                          (recents  . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

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
(use-package which-key-posframe
  :disabled
  :after which-key
  :config
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-center)
  (which-key-posframe-enable))

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
  :hook (prog-mode . editorconfig-mode)
  :config
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

(use-package focus
  :commands (focus-mode))

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

(use-package zoom
  :commands zoom-mode)

;; Company
(use-package company
  :diminish company-mode
  :defines company-backends
  :hook (after-init . global-company-mode)
  :config
  ;; (company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        ;; company-dabbrev-code-other-buffers t
        company-echo-delay (if (display-graphic-p) nil 0) ; remove annoying blinking
        company-idle-delay .2 ; 0.6
        company-minimum-prefix-length 2
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-limit 12
        company-global-modes
        '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        ;; company-frontends '(company-pseudo-tooltip-frontend
        ;;                     company-echo-metadata-frontend)
        company-transformers '(company-sort-by-occurrence)
        company-backends '(company-yasnippet)))
(use-package company-prescient
  :init (company-prescient-mode 1))
;; TODO remove?
;; (use-package flx)
;; (use-package company-flx
;;   :hook (company-mode . company-flx-mode))
(use-package company-posframe
  :hook (company-mode . company-posframe-mode))
(use-package company-box
  :diminish
  :functions (my-company-box--make-line my-company-box-icons--elisp)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-show-single-candidate t
        company-box-max-candidates 50
        company-box-doc-delay 0.5
        company-box-icons-alist 'company-box-icons-all-the-icons)

  (setq company-box-icons-lsp
        '((1 . fa_text_height) ;; Text
          (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
          (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
          (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
          (5 . (fa_cog :foreground "#FF9800")) ;; Field
          (6 . (fa_cog :foreground "#FF9800")) ;; Variable
          (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
          (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
          (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
          (10 . (fa_cog :foreground "#FF9800")) ;; Property
          (11 . md_settings_system_daydream) ;; Unit
          (12 . (fa_cog :foreground "#FF9800")) ;; Value
          (13 . (md_storage :face font-lock-type-face)) ;; Enum
          (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
          (15 . md_closed_caption) ;; Snippet
          (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
          (17 . fa_file_text_o) ;; File
          (18 . md_refresh) ;; Reference
          (19 . fa_folder_open) ;; Folder
          (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
          (21 . (fa_square :face font-lock-constant-face)) ;; Constant
          (22 . (fa_cube :face font-lock-type-face)) ;; Struct
          (23 . fa_calendar) ;; Event
          (24 . fa_square_o) ;; Operator
          (25 . fa_arrows)) ;; TypeParameter
        )
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

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
            (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
            (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))))

;; Language Server Protocol (LSP)
(use-package lsp-mode
  :diminish lsp-mode
  :commands lsp
  :hook ((ruby-mode
          js2-mode typescript-mode
          python-mode
          elm-mode
          ;; web-mode
          ;; css-mode sass-mode scss-mode
          elixir-mode
          go-mode) . lsp-deferred)
  :config
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil
        flymake-fringe-indicator-position 'right-fringe)
  (add-to-list 'exec-path "~/code/github/elixir-ls/release"))
(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))
(use-package lsp-ui
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
  :defer 5
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dtrt-indent
  :defer 5
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
  :disabled
  :defer t
  :config (adaptive-wrap-prefix-mode))

(use-package dumb-jump
  :commands (dump-jump-go
             dumb-jump-go-other-window
             dump-jump-go-prompt
             dump-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window)
  :config
  (setq dump-jump-force-searcher 'rg
        dumb-jump-selector +completion-engine))

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
  (setq auto-revert-verbose nil
        auto-revert-check-vc-info t)
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
  ;; :config
  ;; (setq visual-fill-column-center-text t
  ;;       ;; visual-fill-column-width
  ;;       ;; take Emacs 26 line numbers into account
  ;;       (+ (if (boundp 'display-line-numbers) 6 0)
  ;;          fill-column)))

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
  :mode "\\.go\\'")
(use-package go-eldoc
  :commands go-eldoc-setup)
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
  :mode ("/\\.rspec\\'" . text-mode)
  :commands (rspec-verify-all
             rspec-rerun
             rspec-verify
             rspec-verify-continue
             rspec-run-last-failed
             rspec-toggle-spec-and-target
             rspec-toggle-spec-and-target-find-example
             rspec-verify-method
             rspec-verify-matching
             rspec-verify-single
             rspec-toggle-example-pendingness
             rspec-dired-verify
             rspec-dired-verify-single)
  ;; :hook (ruby-mode . rspec-mode)
  :config
  (setq compilation-scroll-output 'first-error
        rspec-autosave-buffer t)
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
  :requires ruby-mode
  :commands (ruby-hash-syntax-toggle))
(use-package ruby-refactor
  :commands (ruby-refactor-extract-to-method
             ruby-refactor-extract-local-variable
             ruby-refactor-extract-constant
             ruby-refactor-extract-to-let))
(use-package projectile-rails
  :requires projectile
  :hook (projectile-mode . projectile-rails-on))
;; (use-package ruby-test-mode
;;   :commands (ruby-test-run
;;              ruby-test-run-at-point
;;              ruby-test-toggle-between-implementation-and-specification))
(use-package minitest
  :commands (minitest-verify-all
             minitest-verify
             minitest-rerun
             minitest-verify-single))

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

;; latex
(use-package tex-site
  :ensure auctex
  :mode ( "\\.tex\\'" . latex-mode )
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'rainbow-delimiters-mode))
(use-package latex-preview-pane)
(use-package company-auctex)

;; Emacs Lisp (elisp)
(use-package ielm)
(use-package eros
  :commands (eros-eval-defun eros-eval-last-sexp eros-mode))
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode)
  :commands highlight-quoted-mode)
(use-package macrostep
  :mode ("\\*.el\\'" . emacs-lisp-mode))
(use-package overseer)
(use-package elisp-def
  :disabled)
(use-package elisp-demos
  :disabled)
(use-package flycheck-cask
  :hook (emacs-lisp-mode . flycheck-cask-setup))

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
  (setq ccls-executable "ccls"))
(use-package platformio-mode
  :hook ((c-mode c++-mode) . platformio-conditionally-enable))
(use-package clang-format
  :commands (clang-format))

;; Erlang / Elixir
(use-package erlang
  :mode "\\.erl$")
(use-package elixir-mode
  :mode "\\.exs?"
  :config
  (progn
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'lsp-format-buffer)))
    ;; (add-hook 'elixir-format-hook (lambda ()
    ;;                                 (if (projectile-project-p)
    ;;                                     (setq elixir-format-arguments
    ;;                                           (list "--dot-formatter"
    ;;                                                 (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
    ;;                                   (setq elixir-format-arguments nil))))
    ))
(use-package exunit
  :commands (exunit-verify-all
             exunit-verify-all-in-umbrella
             exunit-verify
             exunit-verify-single
             exunit-rerun))
(use-package alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (setq alchemist-project-compile-when-needed t
        alchemist-test-status-modeline t
        alchemist-test-truncate-lines nil)
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
(use-package alchemist-company
  :ensure nil
  :hook (elixir-mode . (lambda ()
                         (setq-local company-backends '(alchemist-company company-yasnippet)))))
(use-package flycheck-credo
  :hook (elixir-mode . flycheck-credo-setup))
(use-package flycheck-mix
  :hook (elixir-mode . flycheck-mix-setup))
;; (use-package flycheck-mix
;;   :commands (flycheck-mix-setup)
;;   :init
;;   (progn
;;     (add-to-list 'safe-local-variable-values
;;                  (cons 'elixir-enable-compilation-checking nil))
;;     (add-to-list 'safe-local-variable-values
;;                  (cons 'elixir-enable-compilation-checking t))
;;     (add-hook 'elixir-mode-local-vars-hook
;;               'spacemacs//elixir-enable-compilation-checking)))

(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode))
(use-package ensime
  :hook (scala-mode . ensime-mode))
(use-package sbt-mode
  :hook (scala-mode . sbt-mode))

(use-package add-node-modules-path
  :hook ((js2-mode js-mode json-mode typescript-mode elm-mode) . add-node-modules-path))
(use-package js2-mode
  :mode "\\.m?js\\'"
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :config
  (setq-default js-switch-indent-offset 2
                js-indent-level 2)
  (setenv "NODE_NO_READLINE" "1"))
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2
        typescript-expr-indent-offset 2))

;; Elm
;; NOTE watch for the release of an LSP for Elm (none as of 2019-05)
(use-package elm-mode
  :mode ("\\.elm\\'" . elm-mode)
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
  :mode ("\\.rei?\\'" . reason-mode)
  :config
  (add-hook 'reason-mode-hook (lambda ()
                                (add-hook 'before-save-hook 'reason/refmt-before-save nil t))))
(use-package merlin
  :after reason-mode
  :config
  (setq merlin-completion-with-doc t))
(use-package flycheck-ocaml
  :after reason-mode)

(use-package company-web
  :requires company
  :hook (web-mode . (lambda ()
                      (setq-local company-backends '(company-web-html company-css company-yasnippet)))))
(use-package web-mode
  :hook (html-mode . web-mode)
  :diminish
  :config
  (setq   web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-block-padding 0
          web-mode-enable-block-face t
          web-mode-enable-current-column-highlight t
          web-mode-auto-close-style 2
          web-mode-enable-html-entities-fontification t
          web-mode-enable-current-element-highlight t
          web-mode-enable-auto-quoting t
          web-mode-enable-auto-pairing t)
  (add-hook 'web-mode-hook #'turn-off-smartparens-mode))
(use-package emmet-mode
  :diminish
  :hook ((css-mode web-mode) . emmet-mode))

;; NOTE does not work yet due to TRAMP stuff
;; (use-package prettier
;;   :quelpa
;;   (prettier :fetcher github :repo "jscheid/prettier.el")
;;   :init (global-prettier-mode 1))

(use-package css-mode
  :mode "\\.css$"
  :hook (css-mode . (lambda ()
                      (setq-local company-backends '(company-css company-yasnippet))))
  :custom
  (css-indent-offset 2))
(use-package scss-mode
  :mode "\\.scss$")
(use-package sass-mode
  :mode "\\.sass$")

(use-package web-beautify
  :commands web-beautify-buffer
  :quelpa
  (web-beautify :fetcher github :repo "jguenther/web-beautify" :branch "add-web-beautify-buffer-cmd"))

(with-eval-after-load 'smartparens
  (sp-local-pair 'web-mode "<" ">")
  (sp-with-modes '(css-mode scss-mode)
    (sp-local-pair "/*" "*/"
                   :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))))

;; Syntax Checking - Flycheck
(use-package flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :pin melpa
  :init (global-flycheck-mode)
  :config
  (setq flycheck-rubocop-lint-only t
        flycheck-idle-change-delay 1.75
        flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq-default flycheck-disabled-checkers '(ruby-rubylint
                                             emacs-lisp-checkdoc))
  (global-flycheck-mode +1))
(use-package flycheck-posframe
  :commands flycheck-posframe-show-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))

;; Ansible
(use-package ansible)
(use-package ansible-doc)

;; Spell Checking
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

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-lsp t))

(use-package hide-mode-line
  :hook (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))

(use-package icons-in-terminal
  :disabled
  :quelpa (icons-in-terminal :fetcher github :repo "seagle0128/icons-in-terminal.el"))

(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.ipynb" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub$" all-the-icons-faicon "book" :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-blue)))

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))


(use-package doom-themes
  :config
  (setq doom-treemacs-enable-variable-pitch t)
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

(use-package base16-theme)

(use-package hide-mode-line
  :hook ((neotree-mode
          treemacs-mode
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

;; Git
(use-package magit)
(use-package evil-magit
  :after magit)

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

(use-package vterm ; https://github.com/akermu/emacs-libvterm
  :commands (vterm vterm-other-window)
  :load-path "~/code/github/emacs-libvterm")
(use-package eshell
  :config
  (setq eshell-visual-commands '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim")
        eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))
        eshell-history-size 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-output 'this
        eshell-scroll-to-bottom-on-input 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t))
(use-package eshell-toggle
  :commands (eshell-toggle)
  :config
  (setq eshell-toggle-use-projectile-root t))
(use-package helpful
  :commands (helpful-callable
             helpful-command
             helpful-variable
             helpful-key
             helpful-macro
             helpful-function)
  :defer t
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
  :mode ("\\.adoc?\\'" . adoc-mode))

(require '+org)

(use-package po-mode
  :mode ("\\.pot?\\'" . po-mode))

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
      frame-resize-pixelwise t

      find-file-visit-truename t ; resolve symlinks when opening files

      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      idle-update-delay 1 ; update UI slightly less often (0.5 default)
      create-lockfiles nil
      cua-mode t
      desktop-save-mode nil
      indent-tabs-mode nil

      make-backup-files nil ; don't create backup~ files

      ;; Use org mode in scratch buffer
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode

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

;; relegate tooltips to echo area only
(if (boundp 'tooltip-mode) (tooltip-mode -1))

;; Handle ANSI codes in compilation buffer
(add-hook 'compilation-filter-hook #'doom|apply-ansi-color-to-compilation-buffer)

;; This is MUCH faster than using set-face-attribute
(add-to-list 'default-frame-alist '(font . "Fira Code:13"))

(load "~/.emacs.secrets" t)
(provide 'config)
;;; config.el ends here
