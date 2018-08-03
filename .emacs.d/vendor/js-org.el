;;; js-org.el --- Part of my Emacs configuration

;;; Commentary:

;;; Code:

(use-package org
  :ensure nil
  :defer 5
  :custom
  (org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                       (sequence "⚑ WAITING(w)" "|")
                       (sequence "|" "✘ CANCELED(c)"))))

;; (use-package org-bullets
;;   :hook
;;   (org-mode . org-bullets-mode))

(use-package org-projectile
  ;; :defer 5
  ;; :after org
  :hook (projectile-before-switch-project-hook . org-projectile-per-project)
  :config
  (progn
    (setq org-projectile-per-project-filepath "TODO.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))
  :general
  (space-leader-def 'normal
    "o c" 'org-capture
    "p c" 'org-projectile-projectile-project-todo-completing-read))



(provide 'js-org)
;;; js-org.el ends here
