;;; deprecate.el --- Packages I want to stop using -*- lexical-binding: t; -*-

;;; Commentary:
;;; These are packages I want to stop using and have to learn the alternatives for.

;;; Code:

(use-package neotree ; Try to use dired instead
  :commands (neotree-toggle
             neotree-projectile-action
             neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :config
  (js|neotree-keybindings)
  (setq neo-create-file-auto-open t
        neo-point-auto-indent nil
        neo-modern-sidebar t
        neo-theme 'icons
        neo-window-fixed-size nil
        neo-window-width 28
        neo-show-hidden-files nil
        neo-keymap-style 'concise))

(use-package treemacs
  ;; :requires all-the-icons
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :config
  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

  (setq treemacs-display-in-side-window        t
        ;; treemacs-file-event-delay              5000
        ;; treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        ;; treemacs-git-command-pipe              ""
        ;; treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   1
        ;; treemacs-indentation-string            " "
        ;; treemacs-indentation-string (propertize " " 'face 'font-lock-comment-face)

        treemacs-is-never-other-window         t
        ;; treemacs-max-git-entries               5000
        ;; treemacs-no-png-images                 nil
        ;; treemacs-no-delete-other-windows       t
        ;; treemacs-project-follow-cleanup        nil
        ;; treemacs-recenter-distance             0.1
        ;; treemacs-recenter-after-file-follow    nil
        ;; treemacs-recenter-after-tag-follow     nil
        ;; treemacs-recenter-after-project-jump   'always
        ;; treemacs-recenter-after-project-expand 'on-distance
        ;; treemacs-show-cursor                   nil
        ;; treemacs-show-hidden-files             t
        ;; treemacs-silent-filewatch              nil
        ;; treemacs-silent-refresh                nil
        ;; treemacs-sorting                       'alphabetic-desc
        ;; treemacs-space-between-root-nodes      t
        ;; treemacs-tag-follow-cleanup            t
        ;; treemacs-tag-follow-delay              1.5
        treemacs-width                         28)
  )

(use-package treemacs-evil
  :after treemacs evil)
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

(provide 'deprecate)
