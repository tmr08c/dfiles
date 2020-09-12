;;; org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;(use-package org-plus-contrib)
(use-package org
	:straight (org :type git :host github :repo "emacsmirror/org" :no-build t)
  ;; :ensure org-plus-contrib
  ;; :mode "\\.org\'"
  :config
  ;; (add-hook 'before-save-hook 'langtool-check)
  (add-hook 'org-mode-hook 'variable-pitch-mode)

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; (add-hook 'org-mode-hook 'refill-mode)
  ;; (add-hook 'org-mode-hook 'visual-fill-column-mode)

  (setq-local line-spacing 0.2)

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

  (setq org-agenda-files (list "~/org/gcal-p.org" "~/org/inbox.org"))

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
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme)))

(use-package ox-pandoc
  :after org)
;; (use-package ox-minutes
;;   :after org)
(use-package ox-gfm
  :after org)
(use-package ox-asciidoc
  :after org)
(use-package org-make-toc
  :after org
  :hook (org-mode . org-make-toc-mode)
  :commands (org-make-toc org-make-toc-at-point))
(use-package toc-org
  :disabled
  :custom (toc-org-max-depth 10)
  :hook (org-mode . toc-org-enable))
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")))
(use-package org-bullets
  :disabled
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))
(use-package org-autolist ; RET starts new list entry
  :hook (org-mode . org-autolist-mode))


;; (use-package org-gcal
;;   :after org
;;   :config
;;   (setq org-gcal-client-id "784734461161-i9ggdi0n5ct4qb6vo0ftbjs3n90pdd1a.apps.googleusercontent.com"
;;         org-gcal-file-alist '(("justin.smestad@gmail.com" . "~/org/gcal-p.org"))
;;         org-gcal-auto-archive nil
;;         org-gcal-notify-p nil)
;;   (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
;;   (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch))

;; (use-package org-projectile
;;   :commands org-projectile-projectile-project-todo-completing-read
;;   :functions org-projectile-todo-files
;;   :hook (projectile-before-switch-project-hook . org-projectile-per-project)
;;   :config
;;   (setq org-projectile-per-project-filepath "TODO.org"
;;         org-agenda-files (append org-agenda-files (org-projectile-todo-files))))


(set-face-attribute 'variable-pitch nil
                    :family "Ubuntu" :weight 'regular)

(set-face-attribute 'fixed-pitch nil
                    :family "Cascadia Code" :weight 'regular)

(face-spec-set 'default
               '((((type x)) :family "Cascadia Code" :height 120)
                 (((type ns)) :family "Cascadia Code" :height 120)
                 (t :family :height 120)))

(provide '+org)
;;; +org.el ends here
