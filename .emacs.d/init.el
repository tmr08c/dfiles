;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
;; (add-to-list 'default-frame-alist '(menu-bar-lines . 0)) ;; I want the top level menu, it's good for discovery
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system 'utf-8)     ; please

;; Ensure Emacs is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

(defvar js|config-file
  (expand-file-name "config.el" user-emacs-directory)
  "The file path of your literate config file.")

;;; Add load path for vendor directory
(add-to-list 'load-path "~/.emacs.d/vendor/")

;;; Get package repos configured
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq package-archive-priorities '(("org" . 3)
                                   ("melpa" . 2)
                                   ("gnu" . 1)))

(unless package--initialized
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-compute-statistics t
      use-package-always-ensure t
      ;; use-package-always-defer t
      use-package-verbose t
      use-package-minimum-reported-time 0.01)

(eval-when-compile
  (require 'use-package))

(use-package quelpa
  :custom
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :after quelpa
  :demand
  :config
  (quelpa-use-package-activate-advice))

(use-package hydra)
(use-package use-package-hydra
  :after hydra
  :demand)
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

(use-package auto-package-update
  :requires no-littering
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-prompt-before-update t)
  (apu--last-update-day-filename
   (no-littering-expand-var-file-name "auto-update-package-last-update-day")))


(when (file-readable-p (concat user-emacs-directory "config.el"))
  (load-file (concat user-emacs-directory "config.el")))

(provide 'init)
;;; init.el ends here
