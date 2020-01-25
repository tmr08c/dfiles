;;; +helm.el --- Handles all the Helm related packages -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ------------------------------
;; Helm
;; ------------------------------
(use-package helm
  :diminish helm-mode
  :demand
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action)
              ("TAB" . helm-execute-persistent-action)
              ("C-z" . helm-select-action))

  :bind (:map helm-find-files-map
              ("<tab>" . helm-execute-persistent-action)
              ("S-<tab>" . helm-find-files-up-one-level)
              ("<backtab>" . helm-find-files-up-one-level)
              ("S-TAB" . helm-find-files-up-one-level))
  :config
  (setq helm-candidate-number-limit 50
        ;; Remove extraineous helm UI elements
        helm-display-header-line nil
        helm-mode-line-string nil
        helm-find-files-doc-header nil

        helm-M-x-requires-pattern nil

        ;; Helm Files
        helm-ff-auto-update-initial-value nil
        helm-ff-skip-boring-files t
        helm-ff-initial-sort-method 'newest

        ;; Default helm window sizes
        helm-display-buffer-width nil
        helm-display-buffer-height 0.25))

(use-package helm-rg
  :after helm)
(use-package helm-dash
  :after helm)
(use-package helm-company
  :after (helm company))

(use-package helm-org
  :after helm)
(use-package helm-projectile
  :after helm projectile
  :commands (helm-projectile-find-file
             helm-projectile-find-dir
             helm-projectile-find-file-dwim
             helm-projectile-find-file-in-known-projects
             helm-projectile-recentf
             helm-projectile-grep
             helm-projectile-rg
             helm-projectile-ag
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
(use-package swiper-helm
  :after helm
  :commands (swiper-helm))
(use-package helm-flx
  :after helm
  :hook (helm-mode . helm-flx-mode))
(use-package helm-themes
  :after helm
  :commands (helm-themes))
(general-define-key :keymaps 'global
                    ;; [remap persp-switch-to-buffer]    '+helm/workspace-mini
                    [remap apropos]                     'helm-apropos
                    [remap bookmark-jump]               'helm-bookmarks
                    [remap execute-extended-command]    'helm-M-x
                    [remap find-file]                   'helm-find-files
                    [remap find-library]                'helm-locate-library
                    [remap locate]                      'helm-locate
                    ;; [remap imenu-anywhere]              'helm-imenu-anywhere
                    [remap imenu]                       'helm-semantic-or-imenu
                    [remap noop-show-kill-ring]         'helm-show-kill-ring
                    ;; [remap projectile-ag]               'helm-projectile-rg
                    ;; [remap projectile-find-dir]         'helm-projectile-find-dir
                    ;; [remap projectile-grep]             'helm-projectile-grep
                    ;; [remap projectile-find-file]        'helm-projectile-find-file
                    ;; [remap projectile-recentf]          'helm-projectile-recentf
                    ;; [remap projectile-switch-project]   'helm-projectile-switch-project
                    ;; [remap projectile-switch-to-buffer] 'helm-projectile-switch-to-buffer
                    [remap recentf-open-files]          'helm-recentf
                    [remap swiper]                      'swiper-helm
                    [remap switch-to-buffer]            'helm-buffers-list
                    [remap yank-pop]                    'helm-show-kill-ring)
(with-eval-after-load 'helm-files
  (general-nmap 'helm-map
    "<tab>" 'helm-execute-persistent-action
    "TAB" 'helm-execute-persistent-action
    "C-z" 'helm-select-action)
  (general-nmap 'helm-find-files-map
    "S-TAB" 'helm-find-files-up-one-level
    "<backtab>" 'helm-find-files-up-one-level
    "S-<tab>" 'helm-find-files-up-one-level))
(js|global-keymap
 "feD" '((lambda ()
           (interactive)
           (helm-find-files user-emacs-directory))
         :wk "search emacs config directory")
 "Ts" 'helm-themes)



(provide '+helm)
;;; +helm.el ends here
