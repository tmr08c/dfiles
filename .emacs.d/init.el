;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 40000000)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (if (version<= "27.0" emacs-version)
                  (add-hook 'after-focus-change-function 'garbage-collect)
                (add-hook 'focus-out-hook 'garbage-collect)))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold most-positive-fixnum))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold 800000))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

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
(setq straight-use-package-by-default t)

(defvar js|config-file
  (expand-file-name "config.el" user-emacs-directory)
  "The file path of your literate config file.")

(straight-use-package 'use-package)

(setq use-package-compute-statistics t
      use-package-minimum-reported-time 0.05)

(eval-when-compile
  ;;; Add load path for vendor directory
  (add-to-list 'load-path "~/.emacs.d/vendor/")
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

(when (file-readable-p (concat user-emacs-directory "config.el"))
  (load-file (concat user-emacs-directory "config.el")))

(provide 'init)
;;; init.el ends here
