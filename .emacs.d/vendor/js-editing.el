;;; js-editing.el --- Part of my Emacs setup

;;; Commentary:

;;; Code:

;; Rainbow Delimiters
;; Highlight matching delimiters with unique colors.
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

;; Adapt to foreign indentation offsets
(use-package dtrt-indent
  :delight
  :custom (dtrt-indent-min-quality 60)
  :init (dtrt-indent-global-mode))

(use-package aggressive-indent
  :hook (
         (emacs-lisp-mode . aggressive-indent-mode)
         ;; (ruby-mode . aggressive-indent-mode)
         (css-mode . aggressive-indent-mode)))

(use-package adaptive-wrap
  :config (adaptive-wrap-prefix-mode))

;; TODO this could be handy at SPC g t c
(use-package goto-chg
  :disabled
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))


(use-package dumb-jump
  :commands (dump-jump-go
             dumb-jump-go-other-window
             dump-jump-go-prompt
             dump-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window)
  :custom
  (dumb-jump-selector 'ivy))

(use-package whitespace
  :hook (before-save . delete-trailing-whitespace)
  :commands (whitespace-mode))

(use-package ws-butler
  :delight
  :defer t
  :config (ws-butler-global-mode))

(use-package autorevert
  :ensure nil
  :defer t
  :delight auto-revert-mode
  :config (global-auto-revert-mode))

(use-package undo-tree
  :delight
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :hook (after-init . global-undo-tree-mode))

(use-package unfill
  :disabled
  :bind ([remap fill-paragraph] . #'unfill-toggle))

(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-shift-width 2)
  (evil-want-integration nil)
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  :general
  (general-define-key
   :states 'insert
   "C-v" 'cua-paste
   "C-c" 'cua-copy-region
   "C-x" 'cua-cut-region
   "C-z" 'undo-tree-undo
   "C-Z" 'undo-tree-redo))

(use-package evil-collection
  :requires evil
  :defer t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-company-use-tng nil)
  (evil-collection-mode-list nil)
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-matchit
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-smartparens
  :disabled
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package evil-escape
  :delight
  :custom
  (evil-escape-delay 0.2)
  :hook (evil-mode . evil-escape-mode))

(use-package evil-goggles
  :delight
  :custom
  (evil-goggles-duration 0.1)
  (evil-goggles-enable-delete nil)
  :hook
  (evil-mode . evil-goggles-mode))

(use-package evil-easymotion
  :delight
  :after evil-mode)

(use-package evil-quickscope
  :delight
  :hook (evil-mode . global-evil-quickscope-mode))

(use-package evil-commentary
  :delight
  :hook (evil-mode . evil-commentary-mode))

(use-package hideshow
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

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this)))

;; Electric Pair Mode
(use-package pair-mode
  :disabled
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package smartparens
  :disabled
  :hook ((elixir-mode . smartparens-strict-mode)
         (js-mode . smartparens-strict-mode)
         (python-mode . smartparens-strict-mode)
         (ruby-mode . smartparens-strict-mode)))

;; This package highlights the cursor every time it jumps abruptedly from a
;; place to another (e.g. when changing windows and so on).
(use-package beacon
  :disabled ;; TODO get this working with company mode
  :delight
  :defer 2
  :config
  (progn
    (setq beacon-blink-when-buffer-changes nil)
    (setq beacon-blink-when-window-changes nil)
    (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
    (beacon-mode 1)))

(use-package evil-string-inflection
  :after evil-mode)

(provide 'js-editing)

;;; js-editing.el ends here
