;;; deprecate.el --- Packages I want to stop using -*- lexical-binding: t; -*-

;;; Commentary:
;;; These are packages I want to stop using and have to learn the alternatives for.

;;; Code:

(use-package neotree ; Try to use dired instead
  :commands (neotree-toggle neotree-projectile-action)
  :config
  (js|neotree-keybindings)
  (setq neo-create-file-auto-open t
        neo-modern-sidebar t
        neo-point-auto-indent nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-fixed-size nil
        neo-window-width 28
        neo-show-hidden-files t
        neo-keymap-style 'concise))

(use-package treemacs
  :requires all-the-icons
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :config
  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

  (setq treemacs-display-in-side-window        t
        ;; treemacs-file-event-delay              5000
        ;; treemacs-file-follow-delay             0.2
        ;; treemacs-follow-after-init             t
        ;; treemacs-git-command-pipe              ""
        ;; treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   1
        ;; treemacs-indentation-string            " "
        treemacs-indentation-string (propertize " " 'face 'font-lock-comment-face)

        ;; treemacs-is-never-other-window         nil
        ;; treemacs-max-git-entries               5000
        ;; treemacs-no-png-images                 nil
        ;; treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-desc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)
  ;; Improve treemacs icons
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (let ((all-the-icons-default-adjust 0)
            (tab-width 1))
        ;; Root icon
        (setq treemacs-icon-root-png
              (concat (all-the-icons-octicon "repo" :height 0.8 :v-adjust -0.2)  " "))
        ;; File icons
        (setq treemacs-icon-open-png
              (concat
               (all-the-icons-octicon "chevron-down" :height 0.8 :v-adjust 0.1)
               "\t"
               (all-the-icons-octicon "file-directory" :v-adjust 0)
               "\t")
              treemacs-icon-closed-png
              (concat
               (all-the-icons-octicon "chevron-right" :height 0.8
                                      :v-adjust 0.1 :face 'font-lock-doc-face)
               "\t"
               (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face)
               "\t"))
        ;; File type icons
        (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
              treemacs-icon-fallback (concat
                                      "\t\t"
                                      (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver
                                                            :height 0.8 :v-adjust 0.0)
                                      "\t")
              treemacs-icon-text treemacs-icon-fallback)

        (dolist (item all-the-icons-icon-alist)
          (let* ((extension (car item))
                 (func (cadr item))
                 (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
                 (icon (apply func args))
                 (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
                 (value (concat "\t\t" icon "\t")))
            (unless (ht-get treemacs-icons-hash (s-replace-regexp "\\?" "" key))
              (ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value))
            (unless (ht-get treemacs-icons-hash (s-replace-regexp ".\\?" "" key))
              (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value)))))))
  )

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
