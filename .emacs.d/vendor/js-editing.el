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
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package adaptive-wrap
  :config (adaptive-wrap-prefix-mode))

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
  :init (evil-mode 1)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-shift-width 2))

(use-package evil-commentary
  :requires evil
  :init (evil-commentary-mode))

(use-package evil-surround
  :requires evil
  :init (global-evil-surround-mode))

(use-package evil-matchit
  :requires evil
  :init (global-evil-matchit-mode 1))

(use-package evil-escape
  :requires evil
  :custom
  (evil-escape-delay 0.2)
  :init (evil-escape-mode))


(provide 'js-editing)

;;; js-editing.el ends here
