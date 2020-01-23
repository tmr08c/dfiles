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
  (use-package helm-dash
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
          ivy-initial-inputs-alist nil
          ivy-use-selectable-prompt t))

  (use-package ivy-prescient
    :hook (ivy-mode . ivy-prescient-mode))

  (use-package ivy-rich
    :after (counsel-projectile all-the-icons ivy counsel)
    :defines (all-the-icons-icon-alist
              all-the-icons-dir-icon-alist
              bookmark-alist)
    :functions (all-the-icons-icon-for-file
                all-the-icons-icon-for-mode
                all-the-icons-icon-family
                all-the-icons-match-to-alist
                all-the-icons-faicon
                all-the-icons-octicon
                all-the-icons-dir-is-submodule)
    :preface
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((buffer (get-buffer candidate))
               (buffer-file-name (buffer-file-name buffer))
               (major-mode (buffer-local-value 'major-mode buffer))
               (icon (if (and buffer-file-name
                              (all-the-icons-match-to-alist buffer-file-name
                                                            all-the-icons-icon-alist))
                         (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name)
                                                      :height 0.9 :v-adjust -0.05)
                       (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust -0.05))))
          (if (symbolp icon)
              (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
            icon))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((path (concat ivy--directory candidate))
               (file (file-name-nondirectory path))
               (icon (cond ((file-directory-p path)
                            (cond
                             ((and (fboundp 'tramp-tramp-file-p)
                                   (tramp-tramp-file-p default-directory))
                              (all-the-icons-octicon "file-directory" :height 0.93 :v-adjust 0.01))
                             ((file-symlink-p path)
                              (all-the-icons-octicon "file-symlink-directory" :height 0.93 :v-adjust 0.01))
                             ((all-the-icons-dir-is-submodule path)
                              (all-the-icons-octicon "file-submodule" :height 0.93 :v-adjust 0.01))
                             ((file-exists-p (format "%s/.git" path))
                              (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.01))
                             (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                                  (apply (car matcher) (list (cadr matcher) :height 0.93 :v-adjust 0.01))))))
                           ((string-match "^/.*:$" path)
                            (all-the-icons-material "settings_remote" :height 0.9 :v-adjust -0.2))
                           ((not (string-empty-p file))
                            (all-the-icons-icon-for-file file :height 0.9 :v-adjust -0.05)))))
          (if (symbolp icon)
              (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
            icon))))
    :hook ((ivy-mode . ivy-rich-mode)
           (ivy-rich-mode . (lambda ()
                              (setq ivy-virtual-abbreviate
                                    (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil)

    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
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
            counsel-switch-buffer
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
            persp-switch-to-buffer
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
              (ivy-read-file-transformer)))
            counsel-file-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-dired
            (:columns
             ((ivy-rich-file-icon)
              (ivy-read-file-transformer)))
            counsel-dired-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-git
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-recentf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 0.8))
              (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
            counsel-bookmark
            (:columns
             ((ivy-rich-bookmark-type)
              (ivy-rich-bookmark-name (:width 40))
              (ivy-rich-bookmark-info)))
            counsel-projectile-switch-project
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-projectile-find-file
            (:columns
             ((ivy-rich-file-icon)
              (counsel-projectile-find-file-transformer)))
            counsel-projectile-find-dir
            (:columns
             ((ivy-rich-file-icon)
              (counsel-projectile-find-dir-transformer)))
            treemacs-projectile
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))))
    (ivy-rich-mode +1))

  (use-package all-the-icons-ivy
    :after ivy
    :config
    ;; let (all-the-icons-ivy-file-commands '(counsel-projectile
    ;;                                          counsel-projectile-find-file
    ;;                                          counsel-projectile-find-dir))
    (all-the-icons-ivy-setup))

  (use-package doom-todo-ivy
	       :straight (doom-todo-ivy :type git :host github :repo "jsmestad/doom-todo-ivy")
    :commands doom/ivy-tasks)

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
    :commands (counsel-dash
               counsel-dash-at-point
               counsel-dash-install-docset)
    :hook
    ((lisp-mode . (lambda () (setq-local counsel-dash-docsets '("Common_Lisp"))))
     (emacs-lisp-mode . (lambda () (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
     (elixir-mode . (lambda () (setq-local counsel-dash-docsets '("Elixir"))))
     (ruby-mode . (lambda () (setq-local counsel-dash-docsets '("Ruby"))))
     (projectile-rails-mode . (lambda () (setq-local counsel-dash-docsets '("Ruby_on_Rails_5"))))
     (sql-mode . (lambda () (setq-local counsel-dash-docsets '("PostgreSQL"))))
     (web-mode . (lambda () (setq-local counsel-dash-docsets '("Javascript" "HTML")))))
    :config
    (setq counsel-dash-browser-func 'eww
          counsel-dash-common-docsets '()
          counsel-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master"))
  )

(provide '+completion)
;;; +completion.el ends here
