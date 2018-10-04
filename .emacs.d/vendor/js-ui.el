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

;;; Theme
(use-package doom-themes
  :demand
  ;; :custom
  ;; (doom-molokai-brighter-comments t)
  :init
  (load-theme 'doom-molokai t)
  (+evil|update-cursor-color))


;; Modeline
(use-package doom-modeline
	:defer t
  :hook (after-init . doom-modeline-init))
(use-package hide-mode-line
  :hook ((neotree-mode . hide-mode-line-mode)
         (completion-list-mode . hide-mode-line-mode)
         (completion-in-region-mode . hide-mode-line-mode)))


;;; Support Emojis in Emacs
(use-package emojify
  :defer 5
  :custom
  (emojify-display-style 'unicode)
  :hook
  ((markdown-mode . emojify-mode)
   (git-commit-mode . emojify-mode)
   (magit-status-mode . emojify-mode)
   (magit-log-mode . emojify-mode)))

;; TODO try out shackle instead
;; (use-package popwin
;;   :defer 3
;;   :hook (after-init . popwin-mode))

(use-package shackle
  :disabled
  :diminish
  :custom
  (shackle-default-alignment 'below)
  (shackle-default-size 8)
  (shackle-rules
   '(
     ;; built-in (emacs)
     ("*compilation*" :size 0.25 :noselect t :autokill t :autoclose t)
     ("*ert*" :same t :modeline t)
     ("*info*" :size 0.5 :select t :autokill t)
     ("*Backtrace*" :size 20 :noselect t)
     ("*Warnings*"  :size 12 :noselect t :autofit t)
     ("*Messages*"  :size 12 :noselect t)
     ("*Help*" :size 0.3 :autokill t)
     (helpful-mode :size 0.3 :autokill t)
     ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
     (apropos-mode :size 0.3 :autokill t :autoclose t)
     (Buffer-menu-mode :size 20 :autokill t)
     (comint-mode :noesc t)
     (grep-mode :size 25 :noselect t :autokill t)
     (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
     (tabulated-list-mode :noesc t)
     ("^ ?\\*" :regexp t :size 15 :noselect t :autokill t :autoclose t))))


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
