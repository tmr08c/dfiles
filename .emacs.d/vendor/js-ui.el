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

;;; Font
;; (set-face-attribute 'default nil
;;                     :family "Fira Mono"
;;                     :height 130
;;                     :weight 'normal
;;                     :width 'normal)

;; This is MUCH faster than using set-face-attribute
(add-to-list 'default-frame-alist '(font . "Fira Mono:13"))

;; (add-to-list 'default-frame-alist '(width . 120))
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; Appearance
;; (if (display-graphic-p)
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
  :defer t
  :init (load-theme 'doom-molokai t))


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
  :init
  (progn
    ;; (if (eq system-type 'darwin)
    (setq emojify-display-style 'unicode)
    ;; (setq emojify-display-style 'image))
    (global-emojify-mode)))

;; TODO try out shackle instead
;; (use-package popwin
;;   :defer 3
;;   :hook (after-init . popwin-mode))

(use-package shackle
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

(provide 'js-ui)

;;; js-ui.el ends here
