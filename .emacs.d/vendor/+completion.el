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
    :after helm
    :config
    (setq helm-ag-base-command "rg --smart-case --no-heading --vimgrep"))
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
          ivy-format-function 'ivy-format-function-arrow
          ivy-initial-inputs-alist nil
          ivy-use-selectable-prompt t))
  ;; (use-package ivy-rich
  ;;   :after ivy
  ;;   :config
  ;;   (setq ivy-virtual-abbreviate 'full
  ;;         ivy-rich-switch-buffer-align-virtual-buffer t
  ;;         ivy-rich-path-style 'abbrev)
  ;;   (ivy-rich-mode 1))
  (use-package ivy-rich
    :after counsel-projectile
    :defines (all-the-icons-mode-icon-alist all-the-icons-dir-icon-alist bookmark-alist)
    :functions (all-the-icons-icon-family
                all-the-icons-match-to-alist
                all-the-icons-auto-mode-match?
                all-the-icons-octicon
                all-the-icons-dir-is-submodule)
    :hook (ivy-rich-mode . (lambda ()
                             (setq ivy-virtual-abbreviate
                                   (or (and ivy-rich-mode 'abbreviate) 'name))))
    :preface
    (with-eval-after-load 'all-the-icons
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(gfm-mode  all-the-icons-octicon "markdown" :v-adjust 0.0 :face all-the-icons-lblue)))

    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (when-let* ((buffer (get-buffer candidate))
                    (major-mode (buffer-local-value 'major-mode buffer))
                    (icon (if (and (buffer-file-name buffer)
                                   (all-the-icons-auto-mode-match? candidate))
                              (all-the-icons-icon-for-file candidate)
                            (all-the-icons-icon-for-mode major-mode))))
          (if (symbolp icon)
              (setq icon (all-the-icons-icon-for-mode 'fundamental-mode)))
          (unless (symbolp icon)
            (propertize icon
                        'face `(
                                :height 1.1
                                :family ,(all-the-icons-icon-family icon)
                                ))))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let ((icon (if (file-directory-p candidate)
                        (cond
                         ((and (fboundp 'tramp-tramp-file-p)
                               (tramp-tramp-file-p default-directory))
                          (all-the-icons-octicon "file-directory"))
                         ((file-symlink-p candidate)
                          (all-the-icons-octicon "file-symlink-directory"))
                         ((all-the-icons-dir-is-submodule candidate)
                          (all-the-icons-octicon "file-submodule"))
                         ((file-exists-p (format "%s/.git" candidate))
                          (all-the-icons-octicon "repo"))
                         (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                              (apply (car matcher) (list (cadr matcher))))))
                      (all-the-icons-icon-for-file candidate))))
          (unless (symbolp icon)
            (propertize icon
                        'face `(
                                :height 1.1
                                :family ,(all-the-icons-icon-family icon)
                                ))))))

    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-buffer-icon (:width 1))
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            ivy-switch-buffer-other-window
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
             ((counsel-describe-function-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-variable
            (:columns
             ((counsel-describe-variable-transformer (:width 50))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
            counsel-find-file
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 30))))
            counsel-file-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 30))))
            counsel-dired-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 30))))
            counsel-git
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 30))))
            counsel-projectile-find-file
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 30))))
            counsel-projectile-find-dir
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 30))))
            counsel-recentf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 90))
              (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
            counsel-bookmark
            (:columns
             ((ivy-rich-bookmark-type)
              (ivy-rich-bookmark-name (:width 40))
              (ivy-rich-bookmark-info)))
            ))
    :init
    (setq ivy-rich-parse-remote-buffer nil)
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
    :config
    (setq counsel-mode-override-describe-bindings t
          counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
          counsel-describe-function-function #'helpful-callable
          counsel-describe-variable-function #'helpful-variable
          counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
          counsel-ag-base-command "ag -S --nocolor --nogroup %s"
          counsel-pt-base-command "pt -S --nocolor --nogroup -e %s"))
  (use-package counsel-projectile
    :commands (counsel-projectile-switch-to-buffer
               counsel-projectile-find-dir
               counsel-projectile-find-file
               counsel-projectile-find-file-dwim
               counsel-projectile-switch-project
               counsel-projectile-grep
               counsel-projectile-git-grep
               counsel-projectile-switch-to-buffer
               counsel-projectile-org-capture
               counsel-projectile-org-agenda
               counsel-projectile-ag
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
  )

(provide '+completion)
;;; +completion.el ends here
