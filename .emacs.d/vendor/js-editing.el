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

;; (use-package ws-butler
;;   :delight
;;   :defer t
;;   :config (ws-butler-global-mode))

(use-package autorevert
  :ensure nil
  :defer t
  :delight auto-revert-mode
  :config (global-auto-revert-mode))

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
  :disabled
  :requires evil
  :defer t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-company-use-tng nil)
  (evil-collection-mode-list '())
  :init
  (evil-mode . evil-collection-init))

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

(use-package multiple-cursors
  :disabled
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this)))

(use-package evil-string-inflection
  :requires evil
  :defer t)

(use-package yasnippet)

(provide 'js-editing)

;;; js-editing.el ends here
