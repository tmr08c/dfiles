;;; +dired.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Always delete and copy recursively
(setq dired-recursive-deletes 'top
      dired-recursive-copies 'always)

(setq delete-by-moving-to-trash t
      dired-use-ls-dired nil)

;; Show directory first
(setq dired-listing-switches "-alh --group-directories-first")

;; Colorful dired
(use-package diredfl
  :init (diredfl-global-mode 1))
;; Show git info in dired
(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Colourful dired
(use-package diredfl
  :init (diredfl-global-mode 1))

;; Shows icons
(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode))

(provide '+dired)
