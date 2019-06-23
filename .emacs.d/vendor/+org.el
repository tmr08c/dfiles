;;; org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :requires langtool
  :defer 3
  :pin org
  :mode "\\.org\'"
  :config
  (progn
    (add-hook 'before-save-hook 'langtool-check)
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

(provide '+org)
;;; +org.el ends here
