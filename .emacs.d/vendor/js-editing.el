;;; js-editing.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Rainbow Delimiters
;; Highlight matching delimiters with unique colors.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Adapt to foreign indentation offsets
(use-package dtrt-indent
  :defer t
  :delight
  :custom (dtrt-indent-min-quality 60)
  :init (dtrt-indent-global-mode))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         ;; (ruby-mode . aggressive-indent-mode)
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
  :custom
  (whitespace-line-column fill-column)
  (whitespace-style
   '(face indentation tabs tab-mark spaces space-mark newline newline-mark
          trailing lines-tail))
  (whitespace-display-mappings
   '((tab-mark ?\t [?› ?\t])
     (newline-mark ?\n [?¬ ?\n])
     (space-mark ?\ [?·] [?.])))
  :config
  (add-hook 'before-save 'delete-trailing-whitespace))

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

(use-package evil
  :ensure t
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

;; (use-package evil-collection
;;   :requires evil
;;   :defer 5
;;   :custom
;;   (evil-collection-setup-minibuffer t)
;;   (evil-collection-company-use-tng nil)
;;   (evil-collection-mode-list '(go-mode
;;                                ivy
;;                                (pdf pdf-view)
;;                                ruby-mode))
;;   :init
;;   (evil-mode . evil-collection-init))

(use-package evil-surround
  :defer 5
  :init (global-evil-surround-mode 1))

(use-package evil-matchit
  :defer 5
  :init (global-evil-matchit-mode))

(use-package evil-escape
  :requires evil
  :delight
  :custom
  (evil-escape-delay 0.2)
  :init
  (evil-escape-mode))

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
  :defer t
  :delight
  :init (global-evil-quickscope-mode 1))

(use-package evil-commentary
  :defer t
  :delight
  :init (evil-commentary-mode))

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

;; (use-package evil-mc
;;   :hook (evil-mode . global-evil-mc-mode))
;; (use-package evil-mc-extras
;;   :hook (global-evil-mc-mode . global-evil-mc-extras-mode))

;; (use-package multiple-cursors
;;   :disabled
;;   :bind (("C->" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)
;;          ("C-*" . mc/mark-all-like-this)))

(use-package evil-string-inflection
  :requires evil
  :defer t)

(use-package smartparens
  :config
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

  ;; smartparens breaks evil-mode's replace state
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)
  (smartparens-global-mode +1))

(use-package yasnippet
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

(provide 'js-editing)

;;; js-editing.el ends here
