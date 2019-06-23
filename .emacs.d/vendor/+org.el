;;; org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :requires langtool
  :pin org
  :mode "\\.org\'"
  :config
  (progn
    (add-hook 'before-save-hook 'langtool-check)
    (add-hook 'org-mode-hook 'variable-pitch-mode)

    ;; (add-hook 'org-mode-hook 'refill-mode)
    ;; (add-hook 'org-mode-hook 'visual-fill-column-mode)

    (setq-local line-spacing 0.1)

    (setq org-src-tab-acts-natively t
          org-src-fontify-natively t
          org-return-follows-link t
          org-startup-indented t
          org-insert-heading-respect-content t
          org-hide-leading-stars t
          org-hide-emphasis-markers t ; hide /.../, *..*, etc
          org-directory "~/org"
          org-M-RET-may-split-line '((item . nil))
          org-default-notes-file (expand-file-name "notes.org" org-directory))

    ;; Configure org-indent to inherit from fixed-pitch to fix the vertical spacing in code blocks.
    (org-indent ((t (:inherit (org-hide fixed-pitch)))))

    (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                              (sequence "⚑ WAITING(w)" "|")
                              (sequence "|" "✘ CANCELED(c)")))))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :custom
  (evil-org-use-additional-insert t)
  (evil-org-key-theme '(textobjects
                        navigation
                        additional
                        todo)))

(use-package ox-pandoc
  :after org)
;; (use-package ox-minutes
;;   :after org)
(use-package ox-gfm
  :after org)
(use-package ox-asciidoc
  :after org)
(use-package toc-org
  :custom (toc-org-max-depth 10)
  :hook (org-mode . toc-org-enable))
(use-package org-bullets
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))
(use-package org-autolist ; RET starts new list entry
  :hook (org-mode . org-autolist-mode))


(use-package org-projectile
  :commands org-projectile-projectile-project-todo-completing-read
  :functions org-projectile-todo-files
  :hook (projectile-before-switch-project-hook . org-projectile-per-project)
  :config
  (setq org-projectile-per-project-filepath "TODO.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

;; Org Fonts / UI
(let* ((variable-font '(:font "Fira Sans"))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-font))))
   `(org-level-7 ((t (,@headline ,@variable-font))))
   `(org-level-6 ((t (,@headline ,@variable-font))))
   `(org-level-5 ((t (,@headline ,@variable-font))))
   `(org-level-4 ((t (,@headline ,@variable-font :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-font :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-font :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-font :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-font :height 2.0 :underline nil))))))

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 1.3 :weight 'light)

(set-face-attribute 'fixed-pitch nil
                    :family "Fira Code" :weight 'medium)

(provide '+org)
;;; +org.el ends here