;;; config.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defvar +completion-engine 'ivy
  "Setting to control whether to use helm or ivy.")

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system 'utf-8)     ; please

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package which-key
  :defer 1
  :delight
  :init (which-key-mode)
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-max-width 0.33)
  (which-key-setup-side-window-right-bottom))

(use-package general
  :demand t
  :functions space-leader-def
  ;; :custom
  ;; (general-default-prefix "SPC")
  ;; (general-default-non-normal-prefix "C-SPC")
  :config
  (general-create-definer space-leader-def
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-evil-setup)
  (space-leader-def
    ;; :states '(normal visual insert emacs)
    :states '(normal visual emacs)

    "SPC" '(execute-extended-command :which-key "M-x")
    ;; "TAB" '(switch-to-other-buffer :which-key "prev buffer")

    ;;; Help bindings
    "h" '(:ignore t :which-key "Help")
    "hdf" '(describe-function :which-key "describe function")
    "hdm" '(describe-mode :which-key "describe modes") ;; TODO: https://framagit.org/steckerhalter/discover-my-major
    "hdv" '(describe-variable :which-key "describe variable")

    ;;; Buffers
    "b"   '(:ignore t :which-key "Buffers")
    "bb" '(switch-buffer :which-key "list buffers")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "prev buffer")
    "bd" '((lambda ()
             (interactive)
             (kill-buffer (current-buffer)))
           :which-key "close current buffer")
    "bs" '((lambda ()
             (interactive)
             (switch-to-buffer (get-buffer-create "*scratch*")))
           :which-key "scratch buffer")

    ;;; Files
    "f"  '(:ignore t :which-key "Files")
    "fD" '((lambda ()
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
    "ff" '(find-file :which-key "find file")
    "fed" '((lambda ()
              (interactive)
              (find-file-existing js|config-file))
            :which-key "open emacs configuration")

    "d" '(:ignore t :which-key "Docs")

    "g" '(:ignore t :which-key "Go to")
    "gd" '(dumb-jump-go :which-key "definition")
    "gD" '(dumb-jump-go-other-window :which-key "definition (other window)")

    ;;; Quit
    "q"  '(:ignore t :which-key "Quit")
    "qq" '(kill-emacs :which-key "quit")
    "qr" '(restart-emacs :which-key "restart")

    ;;; Search
    "s" '(:ignore t :which-key "Search")
    "ss" '(swiper :which-key "search buffer")
    "sS" '(lambda ()
            (interactive)
            (let ((input (if (region-active-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end))
                           (thing-at-point 'symbol t))))
              (swiper input))
            :which-key "search buffer")

    ;; Toggle
    "t" '(:ignore t :which-key "Toggles")

    ;;; Windows
    "w"   '(:ignore t :which-key "Windows")
    "wd" '(delete-window :which-key "close window")
    "w/" '((lambda ()
             (interactive)
             (split-window-horizontally)
             (other-window 1))
           :which-key "split vertical")
    "w-" '((lambda ()
             (interactive)
             (split-window-vertically)
             (other-window 1))
           :which-key "split horizontal")
    "wh" '(evil-window-left :which-key "window left")
    "w<left>" '(evil-window-left :which-key nil)
    "wj" '(evil-window-down :which-key "window down")
    "w<down>" '(evil-window-down :which-key nil)
    "wk" '(evil-window-up :which-key "window up")
    "w<up>" '(evil-window-up :which-key nil)
    "wl" '(evil-window-right :which-key "window right")
    "w<right>" '(evil-window-right :which-key nil)
    "w=" '(balance-windows :which-key "balance window split")

    "x" '(:ignore t :which-key "text")
    "xt" '(:ignore t :which-key "transpose")
    ))

(defmacro js|global-keymap (&rest bindings)
  "Add global BINDINGS as key bindings under `space-leader-def`.
All of the arguments are treated exactly like they are in
'general' package."
  `(space-leader-def
     :states '(normal visual emacs)
     ,@bindings))

(defmacro js|keymap-for-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under `space-leader-def` for MODE.
mode should be a quoted symbol corresponding to a valid major mode.
the rest of the arguments are treated exactly like they are in
'general' package."
  (let (mode-bindings)
    (while key
      (push def mode-bindings)
      (push (concat "m" key) mode-bindings)
      (setq key (pop bindings) def (pop bindings)))
    `(space-leader-def
       :states '(normal visual emacs)
       :keymaps ',(intern (format "%s-map" (eval mode)))
       ,@mode-bindings)))

(defmacro evil-js|keymap-for-mode (mode &rest bindings)
  "Add BINDINGS to evil for the provided MODE.
mode should be a quoted symbol corresponding to a valid major mode.
the rest of the arguments are treated exactly like they are in
'general' package."
  `(general-define-key
    :states '(normal visual)
    :keymaps ',(intern (format "%s-map" (eval mode)))
    ,@bindings))

(use-package evil
  :init (evil-mode 1)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-shift-width 2)
  (evil-want-integration nil)
  :config
  (setq evil-want-visual-char-semi-exclusive t
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
        evil-default-cursor '+evil-default-cursor
        evil-normal-state-cursor 'box
        ;; evil-emacs-state-cursor  '(box +evil-emacs-cursor)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; Change the cursor color in emacs mode
  (defvar +evil--default-cursor-color
    (or (ignore-errors (frame-parameter nil 'cursor-color))
        "#ffffff"))

  (defun +evil-default-cursor () (set-cursor-color +evil--default-cursor-color))
  (defun +evil-emacs-cursor () (set-cursor-color (face-foreground 'warning)))

  (defun +evil|update-cursor-color ()
    (setq +evil--default-cursor-color (face-background 'cursor)))
  (add-hook 'doom-load-theme-hook #'+evil|update-cursor-color)
  (defun +evil|update-shift-width ()
    (setq evil-shift-width tab-width))
  (add-hook 'after-change-major-mode-hook #'+evil|update-shift-width t)
  :general
  (general-define-key
   :states 'insert
   "C-v" 'cua-paste
   "C-c" 'cua-copy-region
   "C-x" 'cua-cut-region
   "C-z" 'undo-tree-undo
   "C-Z" 'undo-tree-redo))

(use-package evil-escape
  :requires evil
  :init (evil-escape-mode 1)
  :delight
  :custom
  (evil-escape-delay 0.2))

(use-package evil-surround
  :defer 5
  :init (global-evil-surround-mode 1))

(use-package evil-matchit
  :defer 5
  :init (global-evil-matchit-mode))

(use-package evil-goggles
  :defer 5
  :delight
  :custom
  (evil-goggles-duration 0.1)
  (evil-goggles-enable-delete nil)
  :init
  (evil-goggles-mode))

(use-package evil-easymotion
  :defer 5
  :delight)

(use-package evil-quickscope
  :disabled
  :defer t
  :delight
  :init (global-evil-quickscope-mode 1))

(use-package evil-commentary
  :defer t
  :delight
  :init (evil-commentary-mode))

(use-package evil-string-inflection
  :disabled
  :requires evil
  :defer t)

(use-package editorconfig
  :defer t
  :config (editorconfig-mode 1))

(use-package projectile
  :commands (projectile-run-shell-command-in-root
             projectile-replace-regexp
             projectile-toggle-between-implementation-and-test
             projectile-invalidate-cache
             projectile-replace
             projectile-kill-buffers
             projectile-recentf)
  :delight ;;'(:eval (concat " " (projectile-project-name)))
  :init
  (js|global-keymap
   "p"  '(:ignore t :which-key "Projects")
   "p!" '(projectile-run-shell-command-in-root :which-key "run command")
   "p%" '(projectile-replace-regexp :which-key "replace regexp")
   ;; "p a" '(projectile-toggle-between-implementation-and-test :which-key "toggle test")
   "pI" '(projectile-invalidate-cache :which-key "clear cache")
   "pR" '(projectile-replace :which-key "replace")
   "pk" '(projectile-kill-buffers :which-key "kill buffers")
   "pr" '(projectile-recentf :which-key "recent files")
   "pb" '(projectile-switch-to-buffer :which-key "switch to buffer")
   "pd" '(projectile-find-dir :which-key "find directory")
   "pf" '(projectile-find-file :which-key "open file")
   "pp" '(projectile-switch-project :which-key "open project")
   "ps" '(projectile-ag :which-key "search in project"))
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

;; (use-package helm)
;; (use-package helm-ag)
;; (use-package helm-company)
;; (use-package helm-projectile)
;; (use-package swiper-helm)
;; (use-package helm-flx)

(use-package ivy
  :if (eq +completion-engine 'ivy)
  :demand
  :delight
  :general
  (general-define-key :keymaps 'global
                      ;; [remap switch-to-buffer]       #'ivy-switch-buffer
                      ;; [remap persp-switch-to-buffer] #'+ivy/switch-workspace-buffer
                      [remap imenu-anywhere]         #'ivy-imenu-anywhere)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function nil
        ivy-height 15
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ivy-wrap t
        ivy-format-function 'ivy-format-function-line
        ivy-initial-inputs-alist nil
        ivy-use-selectable-prompt t))

(use-package ivy-rich
  :if (eq +completion-engine 'ivy)
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1))

(use-package doom-todo-ivy
  :if (eq +completion-engine 'ivy)
  :ensure nil
  :commands doom/ivy-tasks
  :load-path "vendor/"
  :init
  (js|global-keymap
   "p T" '(doom/ivy-tasks :which-key "List project tasks")))

(use-package counsel
  :if (eq +completion-engine 'ivy)
  :commands (counsel-M-x counsel-find-file counsel-descbinds)
  :init
  (js|global-keymap
   "?" '(counsel-descbinds :which-key "Help")
   "Ts" '(counsel-load-theme :which-key "switch theme"))
  :custom
  (counsel-mode-override-describe-bindings t)
  :general
  (general-define-key :keymaps 'global
                      [remap apropos]                  #'counsel-apropos
                      [remap bookmark-jump]            #'counsel-bookmark
                      [remap describe-face]            #'counsel-faces
                      [remap describe-function]        #'counsel-describe-function
                      [remap describe-variable]        #'counsel-describe-variable
                      [remap execute-extended-command] #'counsel-M-x
                      [remap find-file]                #'counsel-find-file
                      [remap find-library]             #'counsel-find-library
                      [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
                      [remap imenu]                    #'counsel-imenu
                      [remap recentf-open-files]       #'counsel-recentf
                      [remap org-capture]              #'counsel-org-capture
                      [remap swiper]                   #'counsel-grep-or-swiper))

(use-package counsel-projectile
  :if (eq +completion-engine 'ivy)
  :commands (counsel-projectile-switch-to-buffer
             counsel-projectile-find-dir
             counsel-projectile-find-file
             counsel-projectile-switch-project
             counsel-projectile-rg)
  :general
  (general-define-key :keymaps 'global
                      [remap projectile-find-file]        #'+ivy/projectile-find-file
                      [remap projectile-find-dir]         #'counsel-projectile-find-dir
                      [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
                      [remap projectile-grep]             #'counsel-projectile-grep
                      [remap projectile-ag]               #'counsel-projectile-rg
                      [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  )


(use-package counsel-dash
  :if (eq +completion-engine 'ivy)
  :commands counsel-dash
  :hook
  ((lisp-mode . (lambda ()
                  (setq-local counsel-dash-docsets '("Common_Lisp"))))
   (emacs-lisp-mode . (lambda ()
                        (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
   (ruby-mode . (lambda ()
                  (setq-local counsel-dash-docsets '("Ruby"))))
   (projectile-rails-mode . (lambda ()
                              (setq-local counsel-dash-docsets '("Ruby_on_Rails_5"))))
   (sql-mode . (lambda ()
                 (setq-local counsel-dash-docsets '("PostgreSQL"))))
   (web-mode . (lambda ()
                 (setq-local counsel-dash-docsets '("Javascript" "HTML")))))
  :custom
  (counsel-dash-browser-func 'eww)
  (counsel-dash-common-docsets '())
  :init
  (js|global-keymap
   "dd" '((lambda ()
            (interactive)
            (counsel-dash
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (substring-no-properties (or (thing-at-point 'symbol) "")))))
          :which-key "Lookup thing at point")
   "dD" '(counsel-dash :which-key "Lookup thing at point with docset")
   ))

(use-package counsel-etags
  :if (eq +completion-engine 'ivy)
  :requires counsel
  :commands (counsel-etags-find-tag-at-point
             counsel-etags-scan-code
             counsel-etags-grep
             counsel-etags-grep-symbol-at-point
             counsel-etags-recent-tag
             counsel-etags-find-tag
             counsel-etags-list-tag))

(use-package company
  :defer t
  :delight
  :defines company-backends
  :hook (after-init . global-company-mode)
  :custom
  ;; (company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-code-other-buffers t)
  (company-echo-delay 0) ; remove annoying blinking
  (company-idle-delay 0.6)
  (company-minimum-prefix-length 2)
  (company-require-match 'never)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 14)
  (company-global-modes
   '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode))
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  (company-transformers '(company-sort-by-occurrence))
  (company-backends '()))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (prescient-persist-mode +1))

;; (use-package company-quickhelp
;;   :hook (company-mode . company-quickhelp-mode)
;;   :custom
;;   (company-quickhelp-delay 0.1)
;;   :general
;;   (general-def 'insert company-quickhelp-mode-map
;;     "C-k" 'company-select-previous))

(use-package company-flx
  :hook (company-mode . company-flx-mode))

(use-package lsp-mode
  :hook ((ruby-mode
          js-mode js2-mode
          typescript-mode
          python-mode
          web-mode
          css-mode
          go-mode) . lsp)
  :config
  (require 'lsp-clients)
  (setq lsp-enable-snippet t))
;; :commands (lsp-mode lsp-define-stdio-client)
;; :hook prog-mode
;; :custom
;; (lsp-message-project-root-warning t))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-doc-max-height 8
;;         lsp-ui-doc-max-width 35
;;         lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :hook (lsp-mode))


(custom-set-faces
 '(company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

(use-package neotree
  :commands (neotree-toggle neotree-projectile-action)
  :config
  (setq neo-create-file-auto-open t
        neo-modern-sidebar t
        neo-point-auto-indent nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-fixed-size nil
        neo-window-width 28
        neo-show-hidden-files t
        neo-keymap-style 'concise)
  :init
  (js|global-keymap
   "ft" 'neotree-toggle
   "pt" 'neotree-projectile-action)
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
  :defer t
  :init
  (progn
    (setq indent-guide-delay 0.3)
    (js|global-keymap
     "ti" 'indent-guide-mode
     "t TAB" 'indent-guide-global-mode)))

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
  :custom
  (dumb-jump-selector 'ivy))

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
  :hook (after-init . global-undo-tree-mode))

(use-package unfill
  :disabled
  :bind ([remap fill-paragraph] . #'unfill-toggle))

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

(js|global-keymap
 "tc" 'hide/show-comments-toggle)

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
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package highlight-indentation
  :defer 10
  :config
  (js|global-keymap
   "th" '(:ignore t :which-key "highlight")
   "thi" 'highlight-indentation-mode
   "thc" 'highlight-indentation-current-column-mode))

(use-package visual-fill-column
  :config
  (setq-default
   visual-fill-column-center-text t
   visual-fill-column-width
   ;; take Emacs 26 line numbers into account
   (+ (if (boundp 'display-line-numbers) 6 0)
      fill-column)))

(use-package swiper
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
      (setq google-translate-default-target-language (downcase target)))

    (js|global-keymap
     "xg" '(:ignore t :which-key "google translate")
     "xgl" 'spacemacs/set-google-translate-languages
     "xgQ" 'google-translate-query-translate-reverse
     "xgq" 'google-translate-query-translate
     "xgT" 'google-translate-at-point-reverse
     "xgt" 'google-translate-at-point))
  :config
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-enable-ido-completion t
          google-translate-show-phonetic t
          google-translate-default-source-language "en"
          google-translate-default-target-language "de")))

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
  (js|keymap-for-mode 'go-mode
                   "t" '(:ignore t :which-key "test")
                   "ta" '(js|go-run-test-current-suite :which-key "run suite")
                   "tt" '(js|go-run-test-current-function :which-key "run current function")
                   "tg" '(:ignore t :which-key "generate")
                   "tgf" '(go-gen-test-exported :which-key "all exported functions")
                   "tga" '(go-gen-test-all :which-key "all functions")
                   "tgs" '(go-gen-test-dwim :which-key "selected region")

                   ;; Go To
                   "g" '(:ignore t :which-key "goto")
                   "gc" '(go-coverage :which-key "coverage")

                   ;; Imports
                   "i" '(:ignore t :which-key "imports")
                   "ia" '(go-import-add :which-key "add")
                   "ig" '(go-import-add :which-key "goto")
                   "ir" '(go-remove-unused-imports :which-key "remove unused")

                   ;; Execute
                   "x" '(:ignore t :which-key "execute")
                   "xx" '(js|go-run-main :which-key "run main")

                   ;; Refactoring
                   "r" '(:ignore t :which-key "refactoring")
                   "ri" '(go-impl :which-key "implement interface")
                   "rs" '(go-fill-struct :which-key "fill struct")
                   "rd" '(godoctor-godoc :which-key "godoc")
                   "re" '(godoctor-extract :which-key "extract")
                   "rn" '(godoctor-rename :which-key "rename")
                   ;; "rN" '(go-rename :which-key "rename")
                   "rt" '(godoctor-toggle :which-key "toggle")

                   ;; Help
                   "h" '(:ignore t :which-key "help")
                   "hh" '(godoc-at-point :which-key "godoc at point"))
  :custom
  (gofmt-command "goimports")
  ;; :ensure-system-package
  ;; ((gocode . "go get -u github.com/mdempsky/gocode")
  ;;  (gometalinter . "go get -u github.com/alecthomas/gometalinter")
  ;;  (godoc . "go get -u golang.org/x/tools/cmd/godoc")
  ;;  (goimports . "go get -u golang.org/x/tools/cmd/goimports")
  ;;  (guru . "go get -u golang.org/x/tools/cmd/guru"))
  )

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
             go-gen-test-dwim)
  ;; :ensure-system-package
  ;; (gotests . "go get -u github.com/cweill/gotests/...")
  )

(use-package go-fill-struct
  :commands (go-fill-struct)
  ;; :ensure-system-package
  ;; (fillstruct . "go get -u github.com/davidrjenni/reftools/cmd/fillstruct")
  )

(use-package godoctor
  :commands (godoctor-godoc
             godoctor-extract
             godoctor-rename
             godoctor-toggle))

(use-package go-rename
  :commands (go-rename))
  ;; :ensure-system-package
  ;; (gorename . "go get -u golang.org/x/tools/cmd/gorename")


(use-package go-impl
  :commands go-impl)
  ;; :ensure-system-package
  ;; (impl . "go get -u github.com/josharian/impl")


;;;###autoload
(defun js|go-run-tests (args)
  (interactive)
  (compilation-start (concat "go test " args " " go-use-test-args)
                     nil (lambda (n) go-test-buffer-name) nil))

;;;###autoload
(defun js|go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)

    (let ((test-method (if go-use-gocheck-for-testing
                             "-check.f"
                           "-run")))
        (save-excursion
          (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
          (js|go-run-tests (concat test-method "='" (match-string-no-properties 2) "$'"))))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

;;;###autoload
(defun js|go-run-test-current-suite ()
  (interactive)
  (if (string-match "_test\.go" buffer-file-name)
      (if go-use-gocheck-for-testing
          (save-excursion
            (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
            (js|go-run-tests (concat "-check.f='" (match-string-no-properties 2) "'")))
        (message "Gocheck is needed to test the current suite"))
    (message "Must be in a _test.go file to run go-test-current-suite")))


;;;###autoload
(defun js|go-run-main ()
  (interactive)
  (shell-command
   (format "go run %s"
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer)))))))

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
  (js|keymap-for-mode 'ruby-mode
                   "T" '(:ignore t :which-key "toggle")
                   "T'" 'ruby-toggle-string-quotes
                   "T{" 'ruby-toggle-block)
  :custom
  (ruby-insert-encoding-magic-comment nil)
  (ruby-align-to-stmt-keywords
   '(if while unless until begin case for def)))

(use-package bundler
  :hook (ruby-mode . bundler-mode)
  :config
  (js|keymap-for-mode 'ruby-mode
                   "b" '(:ignore t :which-key "bundle")
                   "bc" 'bundle-check
                   "bi" 'bundle-install
                   "bs" 'bundle-console
                   "bu" 'bundle-update
                   "bx" 'bundle-exec
                   "bo" 'bundle-open))

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter-hook . inf-ruby-auto-enter))
  :custom
  (inf-ruby-console-environment "development")
  :config
  (js|keymap-for-mode 'ruby-mode
                   "s" '(:ignore t :which-key "repl")
                   "sb" 'ruby-send-buffer
                   "sB" 'ruby-send-buffer-and-go
                   "sf" 'ruby-send-definition
                   "sF" 'ruby-send-definition-and-go
                   "sl" 'ruby-send-line
                   "sL" 'ruby-send-line-and-go
                   "sr" 'ruby-send-region
                   "sR" 'ruby-send-region-and-go
                   "ss" 'ruby-switch-to-inf))

(use-package company-inf-ruby
  :after inf-ruby
  :config
  (add-to-list 'company-backends 'company-inf-ruby))

;; Not available yet on MELPA
;; (use-package lsp-ruby
;;   :requires lsp-mode
;;   :hook (ruby-mode . lsp-ruby-enable))

;; (use-package robe
;;   :disabled
;;   :hook (ruby-mode . robe-mode)
;;   :config (add-to-list 'company-backends 'company-robe))

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
       :suffix "")))
  (js|keymap-for-mode 'ruby-mode
                      "t" '(:ignore t :which-key "test")
                      "ta"    'rspec-verify-all
                      "tb"    'rspec-verify
                      "tc"    'rspec-verify-continue
                      "td"    'ruby/rspec-verify-directory
                      "te"    'rspec-toggle-example-pendingness
                      "tf"    'rspec-verify-method
                      "tl"    'rspec-run-last-failed
                      "tm"    'rspec-verify-matching
                      "tr"    'rspec-rerun
                      "tt"    'rspec-verify-single
                      "t~"    'rspec-toggle-spec-and-target-find-example
                      "t TAB" 'rspec-toggle-spec-and-target))

(use-package rubocop
  :ensure-system-package
  (rubocop . "gem install rubocop")
  :hook (ruby-mode . rubocop-mode)
  :config
  (js|keymap-for-mode 'ruby-mode
                      "rr" '(:ignore t :which-key "Rubocop")
                      "rrd" 'rubocop-check-directory
                      "rrD" 'rubocop-autocorrect-directory
                      "rrf" 'rubocop-check-current-file
                      "rrF" 'rubocop-autocorrect-current-file
                      "rrp" 'rubocop-check-project
                      "rrP" 'rubocop-autocorrect-project))

(use-package rbenv
  :hook (ruby-mode . global-rbenv-mode))

(use-package yard-mode
  :hook (ruby-mode . yard-mode))

(use-package ruby-hash-syntax
  :requires ruby-mode
  :config
  (js|keymap-for-mode 'ruby-mode
                      "fh" 'ruby-hash-syntax-toggle))

(use-package projectile-rails
  :requires projectile
  :hook (projectile-mode . projectile-rails-on))

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
  :config
  (progn
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
          (json-reformat-region start end))))
    (js|keymap-for-mode 'json-mode
                        "=" 'spacemacs/json-reformat-dwim)))

(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\'")

(use-package markdown-mode
  :mode "\\.md$"
  :hook (markdown-mode . flyspell-mode))

(use-package lispy
  :disabled ; quite frustrating library in evil mode
  :custom
  (lispy-close-quotes-at-end-p t)
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          lisp-mode
          scheme-mode
          clojure-mode) . lispy-mode)
  :config
  (progn
    (defun conditionally-enable-lispy ()
      (when (eq this-command 'eval-expression)
        (lispy-mode 1)))
    (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)))


(use-package sly
  :requires evil
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
    (advice-add #'sly-check-version :before #'+common-lisp*refresh-sly-version)
    (js|keymap-for-mode 'lisp-mode
                        "'" 'sly

                        "h" '(:ignore t :which-key "help")
                        "ha" 'sly-apropos
                        "hb" 'sly-who-binds
                        "hd" 'sly-disassemble-symbol
                        "hh" 'sly-describe-symbol
                        "hH" 'sly-hyperspec-lookup
                        "hm" 'sly-who-macroexpands
                        "hp" 'sly-apropos-package
                        "hr" 'sly-who-references
                        "hs" 'sly-who-specializes
                        "hS" 'sly-who-sets
                        "h<" 'sly-who-calls
                        "h>" 'sly-calls-who

                        "c" '(:ignore t :which-key "compile")
                        "cc" 'sly-compile-file
                        "cC" 'sly-compile-and-load-file
                        "cf" 'sly-compile-defun
                        "cl" 'sly-load-file
                        "cn" 'sly-remove-notes
                        "cr" 'sly-compile-region

                        "e" '(:ignore t :which-key "eval")
                        "eb" 'sly-eval-buffer
                        "ee" 'sly-eval-last-expression
                        "eE" 'sly-eval-print-last-expression
                        "ef" 'sly-eval-defun
                        "eF" 'slime-undefine-function
                        "er" 'sly-eval-region

                        ;; "m g" 'spacemacs/common-lisp-navigation-transient-state/body
                        "m" '(:ignore t :which-key "macro")
                        "me" 'sly-macroexpand-1
                        "mE" 'sly-macroexpand-all

                        "s" '(:ignore t :which-key "repl")
                        "sc" 'sly-mrepl-clear-repl
                        "si" 'sly
                        "sq" 'sly-quit-lisp
                        "sr" 'sly-restart-inferior-lisp
                        "ss" 'sly-mrepl-sync

                        "S" '(:ignore t :which-key "stickers")
                        "Sb" 'sly-stickers-toggle-break-on-stickers
                        "Sc" 'sly-stickers-clear-defun-stickers
                        "SC" 'sly-stickers-clear-buffer-stickers
                        "Sf" 'sly-stickers-fetch
                        "Sr" 'sly-stickers-replay
                        "Ss" 'sly-stickers-dwim

                        "t" '(:ignore t :which-key "trace")
                        "tt" 'sly-toggle-trace-fdefinition
                        "tT" 'sly-toggle-fancy-trace
                        "tu" 'sly-untrace-all)))

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

(use-package python-mode
  :mode "\\.py")
(use-package anaconda-mode
  :hook python-mode)
(use-package pyenv-mode
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions)
  :hook python-mode)

;; C (via irony-mode)
(use-package irony
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode))
  :config
  (progn
    (setq irony-additional-clang-options '("-std=c++11"))
    (setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                                    iron-cdb-libclang))

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  (with-eval-after-load 'smartparens
    (sp-with-modes '(c++-mode objc-mode)
      (sp-local-pair "<" ">"
                     :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                     :post-handlers '(("| " "SPC"))))
    (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
      (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))))

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
  :disabled
  :after irony
  :config
  (progn
    (defun c-mode-before-save-hook ()
      (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
        (call-interactively 'clang-format)))

    (add-hook 'before-save-hook #'c-mode-before-save-hook)))

(use-package arduino-mode
  :after irony
  :config
  (add-to-list 'irony-supported-major-modes 'arduino-mode)
  (add-to-list 'irony-lang-compile-option-alist '(arduino-mode . "c++")))

(use-package erlang
  :mode "\\.erl$")

(use-package elixir-mode
  :defer t
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
  :hook (elixir-mode . alchemist-mode)
  :config
  ;; (js|elixir-keybindings)
  (setq alchemist-project-compile-when-needed t
        alchemist-test-status-modeline nil)
  (js|keymap-for-mode 'elixir-mode
                      "=" 'elixir-format

                      "e" '(:ignore t :which-key "eval")
                      "el" 'alchemist-eval-current-line
                      "eL" 'alchemist-eval-print-current-line
                      "er" 'alchemist-eval-region
                      "eR" 'alchemist-eval-print-region
                      "eb" 'alchemist-eval-buffer
                      "eB" 'alchemist-eval-print-buffer
                      "ej" 'alchemist-eval-quoted-current-line
                      "eJ" 'alchemist-eval-print-quoted-current-line
                      "eu" 'alchemist-eval-quoted-region
                      "eU" 'alchemist-eval-print-quoted-region
                      "ev" 'alchemist-eval-quoted-buffer
                      "eV" 'alchemist-eval-print-quoted-buffer

                      "g" '(:ignore t :which-key "goto")
                      "gt" 'alchemist-project-toggle-file-and-tests
                      "gT" 'alchemist-project-toggle-file-and-tests-other-window
                      "gg" 'alchemist-goto-definition-at-point
                      ;; "." 'alchemist-goto-definition-at-point
                      "gb" 'alchemist-goto-jump-back
                      ;; ","  'alchemist-goto-jump-back
                      "gN" 'alchemist-goto-jump-to-previous-def-symbol
                      "gn" 'alchemist-goto-jump-to-next-def-symbol
                      "gj" 'alchemist-goto-list-symbol-definitions


                      "h" '(:ignore t :which-key "help")
                      "h:" 'alchemist-help
                      "hH" 'alchemist-help-history
                      "hh" 'alchemist-help-search-at-point
                      "hr" 'alchemist-help--search-marked-region

                      "m" '(:ignore t :which-key "mix")
                      "m:" 'alchemist-mix
                      "mc" 'alchemist-mix-compile
                      "mx" 'alchemist-mix-run

                      "s" '(:ignore t :which-key "iex")
                      ;; "'"  'alchemist-iex-run
                      "sc" 'alchemist-iex-compile-this-buffer
                      "si" 'alchemist-iex-run
                      "sI" 'alchemist-iex-project-run
                      "sl" 'alchemist-iex-send-current-line
                      "sL" 'alchemist-iex-send-current-line-and-go
                      "sm" 'alchemist-iex-reload-module
                      "sr" 'alchemist-iex-send-region
                      "sR" 'alchemist-iex-send-region-and-go

                      "t" '(:ignore t :which-key "test")
                      "ta" 'alchemist-mix-test
                      "tb" 'alchemist-mix-test-this-buffer
                      "tB" 'alchemist-project-run-tests-for-current-file
                      "tt" 'alchemist-mix-test-at-point
                      "tF" 'alchemist-project-find-test
                      "tf" 'alchemist-mix-test-file
                      "tn" 'alchemist-test-mode-jump-to-next-test
                      "tN" 'alchemist-test-mode-jump-to-previous-test
                      "tr" 'alchemist-mix-rerun-last-test
                      "ts" 'alchemist-mix-test-stale
                      "tR" 'alchemist-test-toggle-test-report-display

                      "x" '(:ignore t :which-key "execute")
                      "xb" 'alchemist-execute-this-buffer
                      "xf" 'alchemist-execute-file
                      "x:" 'alchemist-execute

                      "c" '(:ignore t :which-key "compile")
                      "cb" 'alchemist-compile-this-buffer
                      "cf" 'alchemist-compile-file
                      "c:" 'alchemist-compile

                      "X" '(:ignore t :which-key "hex")
                      "Xi" 'alchemist-hex-info-at-point
                      "Xr" 'alchemist-hex-releases-at-point
                      "XR" 'alchemist-hex-releases
                      "XI" 'alchemist-hex-info
                      "Xs" 'alchemist-hex-search

                      "o" '(:ignore t :which-key "macroexpand")
                      "ol" 'alchemist-macroexpand-once-current-line
                      "oL" 'alchemist-macroexpand-once-print-current-line
                      "ok" 'alchemist-macroexpand-current-line
                      "oK" 'alchemist-macroexpand-print-current-line
                      "oi" 'alchemist-macroexpand-once-region
                      "oI" 'alchemist-macroexpand-once-print-region
                      "or" 'alchemist-macroexpand-region
                      "oR" 'alchemist-macroexpand-print-region)
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
  (js|javascript-keybindings)
  (setq-default js-switch-indent-offset 2
                js-indent-level 2)
  (setenv "NODE_NO_READLINE" "1"))

;;;###autoload
(defun js|javascript-keybindings ()
  "Define keybindings when working with JavaScript."
  (js|keymap-for-mode 'js2-mode
                      "w" 'js2-mode-toggle-warnings-and-errors

                      "h" '(:ignore t :which-key "help")
                      "g" '(:ignore t :which-key "goto")
                      "r" '(:ignore t :which-key "refactor")

                      "z" '(:ignore t :which-key "folding")
                      "zc" 'js2-mode-hide-element
                      "zo" 'js2-mode-show-element
                      "zr" 'js2-mode-show-all
                      "ze" 'js2-mode-toggle-element
                      "zF" 'js2-mode-toggle-hide-functions
                      "zC" 'js2-mode-toggle-hide-comments))

;;;###autoload
(defun js|typescript-keybindings ()
  "Define keybindings when working with TypeScript."
  (js|keymap-for-mode 'typescript-mode
                      "=" 'spacemacs/typescript-tsfmt-format-buffer

                      "g" '(:ignore t :which-key "goto")
                      "gg" 'lsp-goto-implementation
                      "gt" 'lsp-goto-type-definition
                      "gu" 'xref-find-references

                      "h" '(:ignore t :which-key "help")
                      "hh" 'lsp-describe-thing-at-point
                      "hs" 'lsp-describe-session

                      "r" '(:ignore t :which-key "refactor")
                      "rr" 'lsp-rename
                      ))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :config
  (progn
    (defun spacemacs/typescript-tsfmt-format-buffer ()
      "Format buffer with tsfmt."
      (interactive)
      (if (executable-find "tsfmt")
          (let*  ((extension (file-name-extension (or buffer-file-name "tmp.ts") t))
                  (tmpfile (make-temp-file "~fmt-tmp" nil extension))
                  (coding-system-for-read 'utf-8)
                  (coding-system-for-write 'utf-8)
                  (outputbuf (get-buffer-create "*~fmt-tmp.ts*")))
            (unwind-protect
                (progn
                  (with-current-buffer outputbuf (erase-buffer))
                  (write-region nil nil tmpfile)
                  (if (zerop (apply 'call-process "tsfmt" nil outputbuf nil
                                    (list (format
                                           "--baseDir='%s' --"
                                           default-directory)
                                          tmpfile)))
                      (let ((p (point)))
                        (save-excursion
                          (with-current-buffer (current-buffer)
                            (erase-buffer)
                            (insert-buffer-substring outputbuf)))
                        (goto-char p)
                        (message "formatted.")
                        (kill-buffer outputbuf))
                    (progn
                      (message "Formatting failed!")
                      (display-buffer outputbuf)))
                  (progn
                    (delete-file tmpfile)))))
        (error "tsfmt not found. Run \"npm install -g typescript-formatter\"")))
    (add-hook 'before-save-hook 'spacemacs/typescript-tsfmt-format-buffer)

    (js|typescript-keybindings)

    (setq typescript-indent-level 2
          typescript-expr-indent-offset 2)))

(use-package web-mode
  :mode
  (("\\.html\\'"       . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.php\\'"        . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.inky-erb\\'"   . web-mode)
   ("\\.inky\\'"       . web-mode)
   ("\\.hbs\\'"        . web-mode))
  ;; :bind
  ;; (:map web-mode-map
  ;;       ("," . self-with-space)
  ;;       ("<C-return>" . html-newline-dwim))
  :config
  (add-hook 'web-mode-hook #'turn-off-smartparens-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t))

(use-package company-web
  :hook web-mode
  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package css-mode
  :mode "\\.css\\.erb\\'"
  ;; :bind
  ;; (:map css-mode-map
  ;;       ("," . self-with-space)
  ;;       ("{" . open-brackets-newline-and-indent))
  :custom
  (css-indent-offset 2)
  :config
  (add-to-list 'company-backends 'company-css))

(use-package scss-mode
  :mode "\\.scss$")

(use-package ssass-mode
  :mode "\\.sass$")

(use-package counsel-css
  :disabled
  :hook (css-mode . counsel-css-imenu-setup))

(use-package web-beautify
  :hook web-mode)

(with-eval-after-load 'smartparens
  (sp-with-modes '(css-mode scss-mode less-css-mode stylus-mode)
    (sp-local-pair "/*" "*/"
                   :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :pin melpa
  :custom
  (flycheck-rubocop-lint-only t)
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-disabled-checkers '(ruby-rubylint)))

(use-package flyspell
  :disabled
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (not (eq system-type 'windows-nt))
  :commands flyspell-mode
  :hook
  (text-mode . turn-on-flyspell)
  (prog-mode . flyspell-prog-mode)
  :delight
  :config
  (defun js|flyspell-mode-toggle ()
    "Toggle flyspell mode."
    (interactive)
    (if flyspell-mode
        (flyspell-mode -1)
      (flyspell-mode 1)))

  (js|global-keymap
   "S" '(:ignore t :which-key "Spelling")
   "Sb" 'flyspell-buffer
   "Sn" 'flyspell-goto-next-error
   "tS" 'js|flyspell-mode-toggle)
  :custom
  (flyspell-issue-message-flag nil)
  ;; (ispell-silently-savep t)
  (ispell-program-name (executable-find "aspell"))
  (ispell-list-command "--list")
  (ispell-extra-args '("--sug-mode=ultra"
                       "--lang=en_US"
                       "--dont-tex-check-comments")))
(use-package flyspell-correct
  :disabled
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic))

(use-package flyspell-correct-ivy
  :disabled
  :commands (flyspell-correct-ivy)
  :requires ivy
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package writegood-mode
  :defer t
  :hook (text-mode . writegood-mode))

(customize-set-variable 'user-full-name "Justin Smestad")
(customize-set-variable 'user-mail-address "justin.smestad@gmail.com")

;; Use Github as the standard
;; ref http://hilton.org.uk/blog/source-code-line-length
(setq fill-column 125
      inhibit-startup-screen t
      blink-matching-paren nil
      visible-bell nil
      ring-bell-function 'ignore
      window-resize-pixelwise t
      frame-resize-pixelwise t)

;; This is MUCH faster than using set-face-attribute
(add-to-list 'default-frame-alist '(font . "Fira Code:13"))

;; Appearance
;; Theme Emacs for dark color scheme
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package all-the-icons)

(use-package base16-theme
  :defer t)
(use-package doom-themes
  :demand
  ;; :custom
  ;; (doom-molokai-brighter-comments t)
  :init
  (load-theme 'doom-molokai t)
  (+evil|update-cursor-color))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init))

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

;; TODO try out shackle instead
;; (use-package popwin
;;   :defer 3
;;   :hook (after-init . popwin-mode))

;;; Resize all buffers at once with C-M-= / C-M--
(use-package default-text-scale
  :defer 3
  :init (default-text-scale-mode))

;;; Restart Emacs
(use-package restart-emacs
  :commands restart-emacs)

(use-package winum
  :config
  (progn
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line nil
          winum-keymap nil
          winum-ignored-buffers '(" *which-key*"))
    (defun winum-assign-0-to-neotree ()
      (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
    (add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
    (js|global-keymap "`" 'winum-select-window-by-number
                      ;; "²" 'winum-select-window-by-number
                      "0" 'winum-select-window-0-or-10
                      "1" 'winum-select-window-1
                      "2" 'winum-select-window-2
                      "3" 'winum-select-window-3
                      "4" 'winum-select-window-4
                      "5" 'winum-select-window-5
                      "6" 'winum-select-window-6
                      "7" 'winum-select-window-7
                      "8" 'winum-select-window-8
                      "9" 'winum-select-window-9)
    (winum-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      idle-update-delay 2 ; update ui less often (0.5 default)
      create-lockfiles nil
      cua-mode t
      desktop-save-mode nil
      indent-tabs-mode nil
      initial-scratch-message nil
      load-prefer-newer t
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

(defun doom|apply-ansi-color-to-compilation-buffer ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))
;; Handle ansi codes in compilation buffer
(add-hook 'compilation-filter-hook #'doom|apply-ansi-color-to-compilation-buffer)

(setq-default tab-width 2
              indent-tabs-mode nil)

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
  :custom
  (kept-old-versions 2)
  (kept-new-versions 6)
  (backup-by-copying t)
  (require-final-newline t)
  (delete-old-versions t)
  (version-control t)
  (backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (large-file-warning-threshold (* 20 1000 1000) "20 megabytes."))

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
  :custom
  (dired-dwim-target t "Enable side-by-side `dired` buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired`.")
  (dired-recursive-deletes 'top)
  (delete-by-moving-to-trash t)
  (dired-use-ls-dired nil))

(use-package display-line-numbers
  :ensure nil
  :if (> emacs-major-version 25)
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
  :custom
  (recentf-auto-cleanup 200)
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-filename-handlers '(file-truename abbreviate-file-name))
  (recentf-exclude
   (list #'file-remote-p "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
         "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
         "^/var/folders/.+$" "\\.git/config" "\\.git/COMMIT_EDITMSG"))
  :config
  (progn
    (add-hook 'kill-emacs-hook #'recentf-cleanup)
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (setq recentf-auto-save-timer
          (run-with-idle-timer 600 t 'recentf-save-list))))

(use-package eldoc
  :ensure nil
  :delight
  :hook ((ielm-mode eval-expression-minibuffer-setup) . eldoc-mode))

(use-package eshell
  :commands (eshell eshell-mode)
  :custom
  (eshell-visual-commands '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim"))
  (eshell-visual-subcommands '(("git" "log" "l" "diff" "show")))
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-output 'this)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-buffer-shorthand t)
  (eshell-kill-processes-on-exit t))

(use-package helpful
  :after ivy
  :defer t
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point))
  :config
  (general-define-key
   :keymaps 'global
   [remap describe-function] #'helpful-callable
   [remap describe-command]  #'helpful-command
   [remap describe-variable] #'helpful-variable
   [remap describe-key] #'helpful-key)
  (dolist (cmd '(helpful-callable
                 helpful-variable
                 helpful-function
                 helpful-macro
                 helpful-command))
    (cl-pushnew `(,cmd . "^") ivy-initial-inputs-alist))
  :general
  (js|global-keymap
   "hh" '(:ignore t :which-key "helpful")
   "hhh" 'helpful-at-point
   "hhc" 'helpful-command
   "hhf" 'helpful-callable
   "hhk" 'helpful-key
   "hhm" 'helpful-macro
   "hhv" 'helpful-variable))

;;;###autoload
(defun js|org-keybindings ()
  "Define all keybindings we use in org mode."
  (js|keymap-for-mode
   'org-mode
   "'" 'org-edit-special
   "c" 'org-capture

   ;; Clock
   ;; These keybindings should match those under the "aoC" prefix (below)
   "C" '(:ignore t :which-key "clocks")
   "Cc" 'org-clock-cancel
   "Cd" 'org-clock-display
   "Ce" 'org-evaluate-time-range
   "Cg" 'org-clock-goto
   "Ci" 'org-clock-in
   "CI" 'org-clock-in-last
   "Cj" 'org-clock-jump-to-current-clock
   "Co" 'org-clock-out
   "CR" 'org-clock-report
   "Cr" 'org-resolve-clocks

   "d" '(:ignore t :which-key "dates")
   "dd" 'org-deadline
   "ds" 'org-schedule
   "dt" 'org-time-stamp
   "dT" 'org-time-stamp-inactive
   "ee" 'org-export-dispatch
   "fi" 'org-feed-goto-inbox
   "fu" 'org-feed-update-all

   "a" 'org-agenda

   "p" 'org-priority

   "T" '(:ignore t :which-key "toggles")
   "Tc" 'org-toggle-checkbox
   "Te" 'org-toggle-pretty-entities
   "Ti" 'org-toggle-inline-images
   "Tl" 'org-toggle-link-display
   "Tt" 'org-show-todo-tree
   "TT" 'org-todo
   "TV" 'space-doc-mode
   "Tx" 'org-toggle-latex-fragment

   ;; More cycling options (timestamps, headlines, items, properties)
   "L" 'org-shiftright
   "H" 'org-shiftleft
   "J" 'org-shiftdown
   "K" 'org-shiftup

   ;; Change between TODO sets
   "C-S-l" 'org-shiftcontrolright
   "C-S-h" 'org-shiftcontrolleft
   "C-S-j" 'org-shiftcontroldown
   "C-S-k" 'org-shiftcontrolup

   ;; Subtree editing
   "s" '(:ignore t :which-key "trees/subtrees")
   "sa" 'org-toggle-archive-tag
   "sA" 'org-archive-subtree
   "sb" 'org-tree-to-indirect-buffer
   "sh" 'org-promote-subtree
   "sj" 'org-move-subtree-down
   "sk" 'org-move-subtree-up
   "sl" 'org-demote-subtree
   "sn" 'org-narrow-to-subtree
   "sN" 'widen
   "sr" 'org-refile
   "ss" 'org-sparse-tree
   "sS" 'org-sort

   ;; tables
   "t" '(:ignore t :which-key "tables")
   "ta" 'org-table-align
   "tb" 'org-table-blank-field
   "tc" 'org-table-convert
   "tdc" 'org-table-delete-column
   "tdr" 'org-table-kill-row
   "te" 'org-table-eval-formula
   "tE" 'org-table-export
   "th" 'org-table-previous-field
   "tH" 'org-table-move-column-left

   "ti" '(:ignore t :which-key "insert")
   "tic" 'org-table-insert-column
   "tih" 'org-table-insert-hline
   "tiH" 'org-table-hline-and-move
   "tir" 'org-table-insert-row

   "tI" 'org-table-import
   "tj" 'org-table-next-row
   "tJ" 'org-table-move-row-down
   "tK" 'org-table-move-row-up
   "tl" 'org-table-next-field
   "tL" 'org-table-move-column-right
   "tn" 'org-table-create
   "tN" 'org-table-create-with-table.el
   "tr" 'org-table-recalculate
   "ts" 'org-table-sort-lines

   "tt" '(:ignore t :which-key "toggle")
   "ttf" 'org-table-toggle-formula-debugger
   "tto" 'org-table-toggle-coordinate-overlays

   "tw" 'org-table-wrap-region

   ;; Source blocks / org-babel
   "b" '(:ignore t :which-key "babel")
   "bp" 'org-babel-previous-src-block
   "bn" 'org-babel-next-src-block
   "be" 'org-babel-execute-maybe
   "bo" 'org-babel-open-src-block-result
   "bv" 'org-babel-expand-src-block
   "bu" 'org-babel-goto-src-block-head
   "bg" 'org-babel-goto-named-src-block
   "br" 'org-babel-goto-named-result
   "bb" 'org-babel-execute-buffer
   "bs" 'org-babel-execute-subtree
   "bd" 'org-babel-demarcate-block
   "bt" 'org-babel-tangle
   "bf" 'org-babel-tangle-file
   "bc" 'org-babel-check-src-block
   "bj" 'org-babel-insert-header-arg
   "bl" 'org-babel-load-in-session
   "bi" 'org-babel-lob-ingest
   "bI" 'org-babel-view-src-block-info
   "bz" 'org-babel-switch-to-session
   "bZ" 'org-babel-switch-to-session-with-code
   "ba" 'org-babel-sha1-hash
   "bx" 'org-babel-do-key-sequence-in-edit-buffer
   ;; "b." 'spacemacs/org-babel-transient-state/body

   ;; Multi-purpose keys
   "," 'org-ctrl-c-ctrl-c
   "*" 'org-ctrl-c-star
   "-" 'org-ctrl-c-minus
   "#" 'org-update-statistics-cookies
   "RET"   'org-ctrl-c-ret
   "M-RET" 'org-meta-return
   ;; attachments
   "A" 'org-attach
   ;; insertion
   "i" '(:ignore t :which-key "insert")
   "id" 'org-insert-drawer
   "ie" 'org-set-effort
   "if" 'org-footnote-new
   "ih" 'org-insert-heading
   "iH" 'org-insert-heading-after-current
   "iK" 'spacemacs/insert-keybinding-org
   "il" 'org-insert-link
   "in" 'org-add-note
   "ip" 'org-set-property
   "is" 'org-insert-subheading
   "it" 'org-set-tags

   "x" '(:ignore t :which-key "text")
   ;; region manipulation

   ;; "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
   ;; "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
   ;; "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
   "xo" 'org-open-at-point
   ;; "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
   ;; "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
   ;; "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
   ;; "xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=) )
   ))

(use-package org
  :defer 3
  :pin org
  :mode "\\.org\'"
  :hook (org-mode . org-indent-mode)
  :config
  (progn
    (js|org-keybindings)
    (setq org-src-tab-acts-natively t
          org-insert-heading-respect-content t
          org-src-fontify-natively t
          org-directory "~/org"
          org-default-notes-file (expand-file-name "notes.org" org-directory))
    (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                              (sequence "⚑ WAITING(w)" "|")
                              (sequence "|" "✘ CANCELED(c)")))))

(use-package toc-org
  :custom
  (toc-org-max-depth 10)
  :hook (org-mode . toc-org-enable))

(use-package org-projectile
  :hook (projectile-before-switch-project-hook . org-projectile-per-project)
  :config
  (progn
    (setq org-projectile-per-project-filepath "TODO.org"
          setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (js|global-keymap
     "pc" 'org-projectile-projectile-project-todo-completing-read)))

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
  (setq-default pdf-view-display-size 'fit-page)
  (js|keymap-for-mode 'pdf-view
                      ;; Slicing image
                      "sm" 'pdf-view-set-slice-using-mouse
                      "sb" 'pdf-view-set-slice-from-bounding-box
                      "sr" 'pdf-view-reset-slice
                      ;; Annotations
                      "a" '(:ignore t :which-key "annotations")
                      "aD" 'pdf-annot-delete
                      "at"	'pdf-annot-attachment-dired
                      "ah"	'pdf-annot-add-highlight-markup-annotation
                      "al"	'pdf-annot-list-annotations
                      "am"	'pdf-annot-add-markup-annotation
                      "ao"	'pdf-annot-add-strikeout-markup-annotation
                      "as"	'pdf-annot-add-squiggly-markup-annotation
                      "at"	'pdf-annot-add-text-annotation
                      "au"	'pdf-annot-add-underline-markup-annotation
                      ;; Fit image to window
                      "f" '(:ignore t :which-key "fit")
                      "fw" 'pdf-view-fit-width-to-window
                      "fh" 'pdf-view-fit-height-to-window
                      "fp" 'pdf-view-fit-page-to-window
                      ;; Other
                      "s" '(:ignore t :which-key "slice/search")
                      "ss" 'pdf-occur
                      "p" 'pdf-misc-print-document
                      "O" 'pdf-outline
                      "n" 'pdf-view-midnight-minor-mode))

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
