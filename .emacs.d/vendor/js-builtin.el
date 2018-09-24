;;; js-builtin.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; This file contains changes to built-in Emacs packages. Things like
;;; dired, files, etc. Ignore the "use-package" stuff.

;;; Code:

;; Handle very large CSV files
(use-package vlf
  :hook csv-mode)

(use-package window
  :ensure nil
  :preface (provide 'window)
  :custom
  (display-buffer-alist
   `((,(rx bos (or "*Flycheck errors*"
                   "*Backtrace"
                   "*Warnings"
                   "*compilation"
                   "*Help"
                   "*helpful"
                   "*ivy-occur"
                   "*less-css-compilation"
                   "*Packages"
                   "*SQL"))
      (display-buffer-reuse-window
       display-buffer-in-side-window)
      (side            . bottom)
      (reusable-frames . visible)
      (window-height   . 0.5))
     ("." nil (reusable-frames . visible)))))

(use-package files
  :no-require t
  :ensure nil
  :demand t
  :custom
  (backup-by-copying t)
  (require-final-newline t)
  (delete-old-versions t)
  (version-control t)
  (backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (large-file-warning-threshold (* 20 1000 1000) "20 megabytes."))

;; Version control
(use-package vc-hooks
  :no-require t
  :ensure nil
  :demand t
  :custom (vc-follow-symlinks t))


(use-package dired
  :no-require t
  :ensure nil
  :demand t
  :commands (dired)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired` buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired`.")
  (dired-recursive-deletes 'top)
  (delete-by-moving-to-trash t)
  (dired-use-ls-dired nil))

;; Line Numbers
(use-package display-line-numbers
  :ensure nil
  :if (> emacs-major-version 25)
  :hook (prog-mode . display-line-numbers-mode))

;; Fix Annoyances
(use-package uniquify
  :no-require t
  :ensure nil
  :demand t
  :custom (uniquify-buffer-name-style 'forward))

;; Shell
(use-package sh-mode
  :ensure nil
  :mode
  (("\\.zshrc" . sh-mode)
   ("bashrc$" . sh-mode)
   ("bash_profile$" . sh-mode)
   ("bash_aliases$" . sh-mode)
   ("bash_local$" . sh-mode)
   ("bash_completion$" . sh-mode)))

(use-package recentf
  :requires no-littering
  :defer t
  :ensure nil
  :custom
  (recentf-auto-cleanup 200)
  (recentf-max-saved-items 500)
  (recentf-auto-cleanup 'never)
  (recentf-exclude
   '("/tmp/" "\\.ido\\.last" "ido.last" "\\.git/config" "\\.git/COMMIT_EDITMSG"
     "cache/recentf" "\\.emacs\\.d/elpa/.*" "\\.emacs\\.d/.cask/.*" ))
  :config
  (progn
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))))


(use-package eldoc
  :ensure nil
  :delight)

(provide 'js-builtin)

;;; js-builtin.el ends here
