;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold 268435456)

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))


(add-hook 'after-init-hook 'startup/revert-file-name-handler-alist)
(add-hook 'after-init-hook 'startup/reset-gc)
;; (add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
;; (add-hook 'emacs-startup-hook 'startup/reset-gc)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil
      load-prefer-newer t)

