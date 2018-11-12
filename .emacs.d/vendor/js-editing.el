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
