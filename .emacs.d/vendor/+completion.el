;;; +completion.el --- Handles all the Helm / Ivy related packages -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ------------------------------
;; Helm
;; ------------------------------
(when (eq +completion-engine 'helm)

  (use-package helm
    :demand
    :bind (:map helm-map
                ("<tab>" . helm-execute-persistent-action)
                ("TAB" . helm-execute-persistent-action)
                ("C-z" . helm-select-action))
    :config
    (setq helm-candidate-number-limit 50
          helm-display-buffer-height 0.25))
  (use-package helm-files
    :ensure nil
    :after helm
    :bind (:map helm-find-files-map
                ("<tab>" . helm-execute-persistent-action)
                ("S-<tab>" . helm-find-files-up-one-level)
                ("<backtab>" . helm-find-files-up-one-level)
                ("S-TAB" . helm-find-files-up-one-level)))
  (use-package helm-ag
    :after helm)
  (use-package helm-company
    :after (helm company))
  (use-package helm-projectile
    :after helm
    :commands (helm-projectile-find-file
               helm-projectile-find-dir
               helm-projectile-find-file-in-known-projects
               helm-projectile-recentf
               helm-projectile-grep
               helm-projectile-rg
               helm-projectile-ag
               helm-projectile-switch-project
               helm-projectile-switch-to-buffer)
    :config
    (setq projectile-completion-system 'helm))
  (use-package swiper-helm
    :after helm
    :commands (swiper-helm))
  (use-package helm-flx
    :after helm
    :hook (helm-mode . helm-flx-mode))
  (use-package helm-themes
    :after helm
    :commands (helm-themes)))

;; ------------------------------
;; Ivy
;; ------------------------------
(when (eq +completion-engine 'ivy)

  (use-package ivy
    :demand
    :delight
    :config
    (setq ivy-use-virtual-buffers t
          ivy-virtual-abbreviate 'full
          ivy-on-del-error-function nil
          ivy-height 15
          ivy-fixed-height-minibuffer t
          projectile-completion-system 'ivy
          ivy-wrap t
          ivy-format-function 'ivy-format-function-line
          ivy-initial-inputs-alist nil
          ivy-use-selectable-prompt t))
  (use-package ivy-rich
    :after ivy
    :config
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-switch-buffer-align-virtual-buffer t
          ivy-rich-path-style 'abbrev)
    (ivy-rich-mode 1))
  (use-package doom-todo-ivy
    :commands doom/ivy-tasks
    :load-path "vendor/")

  ;; Counsel
  (use-package counsel
    :commands (counsel-M-x
               counsel-find-file
               counsel-descbinds
               counsel-load-theme
               counsel-apropos
               counsel-bookmark
               counsel-faces
               counsel-describe-function
               counsel-describe-variable
               counsel-find-library
               counsel-info-lookup-symbol
               counsel-imenu
               counsel-recentf
               counsel-org-capture
               counsel-grep-or-swiper)
    :custom (counsel-mode-override-describe-bindings t))
  (use-package counsel-projectile
    :commands (counsel-projectile-switch-to-buffer
               counsel-projectile-find-dir
               counsel-projectile-find-file
               counsel-projectile-switch-project
               counsel-projectile-rg))
  (use-package counsel-dash
    :commands counsel-dash
    :hook
    ((lisp-mode . (lambda () (setq-local counsel-dash-docsets '("Common_Lisp"))))
     (emacs-lisp-mode . (lambda () (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
     (ruby-mode . (lambda () (setq-local counsel-dash-docsets '("Ruby"))))
     (projectile-rails-mode . (lambda () (setq-local counsel-dash-docsets '("Ruby_on_Rails_5"))))
     (sql-mode . (lambda () (setq-local counsel-dash-docsets '("PostgreSQL"))))
     (web-mode . (lambda () (setq-local counsel-dash-docsets '("Javascript" "HTML")))))
    :config
    (setq counsel-dash-browser-func 'eww
          counsel-dash-common-docsets '()))
  (use-package counsel-etags
    :after counsel
    :commands (counsel-etags-find-tag-at-point
               counsel-etags-scan-code
               counsel-etags-grep
               counsel-etags-grep-symbol-at-point
               counsel-etags-recent-tag
               counsel-etags-find-tag
               counsel-etags-list-tag)))

(provide '+completion)
;;; +completion.el ends here
