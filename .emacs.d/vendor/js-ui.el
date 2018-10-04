;;; js-ui.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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
(add-to-list 'default-frame-alist '(font . "Fira Mono:13"))

;; Appearance
;; Theme Emacs for dark color scheme
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package all-the-icons
  :commands (all-the-icons-faicon
             all-the-icons-icon-for-buffer
             all-the-icons-icon-for-file
             all-the-icons-icon-for-mode
             all-the-icons-install-fonts))

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
    (global-keymap "`" 'winum-select-window-by-number
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

(provide 'js-ui)

;;; js-ui.el ends here
