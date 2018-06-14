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
  :config (ws-butler-global-mode))

(use-package autorevert
  :ensure nil
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
  (evil-want-integration nil) ;; TODO what is this for? see evil-collection README
  :config
  (fset 'evil-visual-update-x-selection 'ignore))

(use-package evil-collection
  :hook (evil-mode . evil-collection-init))

(use-package evil-commentary
  :delight
  ;; :requires evil
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-surround
  ;; :requires evil
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-matchit
  ;; :requires evil
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-escape
  :delight
  :custom
  (evil-escape-delay 0.2)
  :hook (evil-mode . evil-escape-mode))


(provide 'js-editing)

;;; js-editing.el ends here
