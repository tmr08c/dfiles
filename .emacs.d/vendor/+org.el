;;; org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure org-plus-contrib
  :pin org
  ;; :mode "\\.org\'"
  :config
  ;; (add-hook 'before-save-hook 'langtool-check)
  (add-hook 'org-mode-hook 'variable-pitch-mode)

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

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

  (setq-default org-agenda-dim-blocked-tasks nil
                org-agenda-inhibit-startup t
                org-agenda-skip-unavailable-files t
                ;; Move the agenda to show the previous 3 days and the next 7 days for a bit
                ;; better context instead of just the current week which is a bit confusing
                ;; on, for example, a sunday
                org-agenda-span 10
                org-agenda-start-on-weekday nil
                org-agenda-start-day "-3d")

  (setq org-agenda-files (list "~/org/gcal-k.org" "~/org/gcal-p.org" "~/org/inbox.org"))

  (evil-set-initial-state 'org-agenda-mode 'motion)

  ;; Configure org-indent to inherit from fixed-pitch to fix the vertical spacing in code blocks.
  (require 'org-indent)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (advice-add 'text-scale-adjust :after
              #'visual-fill-column-adjust)

  (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                            (sequence "⚑ WAITING(w)" "|")
                            (sequence "|" "✘ CANCELED(c)"))))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :custom
  ;; (evil-org-use-additional-insert t)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-key-theme '(textobjects
                                    navigation
                                    insert))))

  ;; TODO this rebinds SPC
  ;; (require 'evil-org-agenda)
  ;; (evil-org-agenda-set-keys)
  )

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


(use-package org-gcal
  :after org
  :config
  (setq org-gcal-client-id "784734461161-i9ggdi0n5ct4qb6vo0ftbjs3n90pdd1a.apps.googleusercontent.com"
        org-gcal-file-alist '(("justin.smestad@gmail.com" . "~/org/gcal-p.org")
                              ("justin@keyp.io" . "~/org/gcal-k.org"))
        org-gcal-auto-archive nil
        org-gcal-notify-p nil)
  (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
  (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch))

;; (use-package org-projectile
;;   :commands org-projectile-projectile-project-todo-completing-read
;;   :functions org-projectile-todo-files
;;   :hook (projectile-before-switch-project-hook . org-projectile-per-project)
;;   :config
;;   (setq org-projectile-per-project-filepath "TODO.org"
;;         org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

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
   `(org-document-title ((t (,@headline ,@variable-font :height 2.0 :underline nil))))
   '(org-block                 ((t (:inherit fixed-pitch))))
   '(org-document-info         ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-link                  ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value        ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent                ((t (:inherit (org-hide fixed-pitch)))))))

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 1.3 :weight 'light)

(set-face-attribute 'fixed-pitch nil
                    :family "Fira Code" :weight 'medium)

(provide '+org)
;;; +org.el ends here
