;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; Speed up startup
;; (defvar default-file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)
;; (setq gc-cons-threshold 40000000)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             "Restore defalut values after startup."
;;             (setq file-name-handler-alist default-file-name-handler-alist)
;;             (setq gc-cons-threshold 800000)

;;             ;; GC automatically while unfocusing the frame
;;             ;; `focus-out-hook' is obsolete since 27.1
;;             (if (boundp 'after-focus-change-function)
;;                 (add-function :after after-focus-change-function
;;                               (lambda ()
;;                                 (unless (frame-focus-state)
;;                                   (garbage-collect))))
;;               (add-hook 'after-focus-change-function 'garbage-collect))

;;             ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
;;             ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;             (defun my-minibuffer-setup-hook ()
;;               (setq gc-cons-threshold most-positive-fixnum))

;;             (defun my-minibuffer-exit-hook ()
;;               (setq gc-cons-threshold 800000))

;;             (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;;             (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Prevent the glimpse of un-styled Emacs by setting these early.
;; (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
;; (add-to-list 'default-frame-alist '(menu-bar-lines . 0)) ;; I want the top level menu, it's good for discovery
;; (add-to-list 'default-frame-alist '(vertical-scroll-bars))

(setq frame-title-format '("Emacs |> %b")
      icon-title-format frame-title-format)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system 'utf-8)     ; please

;; Ensure Emacs is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defvar js|config-file
  (expand-file-name "config.el" user-emacs-directory)
  "The file path of your literate config file.")

;; (straight-use-package 'use-package
;;   :pin "4fb1f9a68f1e7e7d614652afc017a6652fd029f1")
;; (straight-use-package '(use-package :type git :host github :repo "jwiegley/use-package" :branch "master"))

(setq straight-use-package-by-default t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0.05)

(eval-when-compile
  ;;; Add load path for vendor directory
  (add-to-list 'load-path "~/.emacs-custom.d/vendor/use-package")
  (add-to-list 'load-path "~/.emacs-custom.d/vendor")
  (require 'use-package))

(use-package hydra)
(use-package hydra-posframe
  :straight (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-mode))
(use-package use-package-hydra
  :after hydra)

(use-package use-package-ensure-system-package
  :demand
  :functions use-package-ensure-system-package-exists?
  :requires (exec-path-from-shell))

(use-package benchmark-init
  :disabled
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package no-littering
  :demand
  :config
  (setq no-littering-var-directory (expand-file-name "var/" user-emacs-directory)
        no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
        custom-file (no-littering-expand-var-file-name "custom.el")))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package which-key
  :hook (after-init . which-key-mode)
  :diminish
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6)
  (which-key-setup-side-window-bottom))

(use-package evil
  :defines (evil-normal-state-map)
  :init (setq evil-want-C-u-scroll t
              evil-want-integration t
              evil-want-keybinding nil) ; This MUST be in init.
  :config
  (setq evil-want-visual-char-semi-exclusive t
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t
        evil-shift-width 2
        evil-magic t
        evil-echo-state t
        evil-search-module 'evil-search
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
        ;; evil-default-cursor '+evil-default-cursor
        evil-normal-state-cursor 'box
        ;; evil-emacs-state-cursor  '(box +evil-emacs-cursor)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Disable Evil for the states below
  (evil-set-initial-state 'Custom-mode 'emacs)

  (defun +evil|update-shift-width ()
    (setq evil-shift-width tab-width))
  (add-hook 'after-change-major-mode-hook #'+evil|update-shift-width t)

  (evil-mode 1))

(use-package general
  :functions space-leader-def
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-auto-unbind-keys)
  (general-create-definer space-leader-def
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-evil-setup)
  (general-vmap "," (general-simulate-key "SPC m"))
  (general-nmap "," (general-simulate-key "SPC m")))

(require '+funcs)

(when (file-readable-p (concat user-emacs-directory "config.el"))
  (load-file (concat user-emacs-directory "config.el")))

(provide 'init)
;;; init.el ends here
