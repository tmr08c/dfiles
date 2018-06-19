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
         (ruby-mode . aggressive-indent-mode)
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
  :custom (undo-tree-auto-save-history t)
  :config (global-undo-tree-mode))

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
  (fset 'evil-visual-update-x-selection 'ignore))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-commentary
  :delight
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-matchit
  ;; :requires evil
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-escape
  :delight
  :custom
  (evil-escape-delay 0.2)
  :hook (evil-mode . evil-escape-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this)))

(provide 'js-editing)

;;; js-editing.el ends here
