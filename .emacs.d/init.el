(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

;; Default to UTF-8 early as this file uses Unicode symbols.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Get package repos configured
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-check-signature nil)
(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


(setq-default use-package-always-defer t
              use-package-always-ensure t
              indent-tabs-mode nil ;; Use spaces instead of tabs
              tab-width 2
              css-indent-offset 2)

(fset 'yes-or-no-p 'y-or-n-p)


(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(setq user-full-name "Justin Smestad"
      user-mail-address "justin.smestad@gmail.com")


;; Platform
;;
(use-package linux
  :ensure nil
  :if (eq system-type 'gnu/linux))

(use-package osx
  :ensure nil
  :if (eq system-type 'darwin))

(use-package reveal-in-osx-finder
  :ensure nil
  :if (eq system-type 'darwin))

(use-package windows
  :ensure nil
  :if (eq system-type 'windows-nt))

;; Auto-update packages.
;;
;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-prompt-before-update ft)
;;   :config (auto-package-update-maybe))


;; Global Modes
;;
;;; Enable which-key
(use-package which-key
  :delight
  :init (which-key-mode)
  :config
    (which-key-setup-side-window-right-bottom)
    (setq which-key-sort-order 'which-key-key-order-alpha
          which-key-side-window-max-width 0.33
          which-key-idle-delay 0.05))

;;; Ivy for completion
(use-package ivy
  :config (progn
            (ivy-mode)
            (setq ivy-use-virtual-buffers t
                  ivy-count-format ""
                  ivy-use-selectable-prompt t)))
;;; Ado-ado
(use-package counsel
  :config (progn
            (global-set-key (kbd "M-x") 'counsel-M-x)))
(use-package swiper)

(use-package flyspell
  :config (progn
            (setq flyspell-issue-message-flag nil)))

;;; Resize all buffers at once with C-M-= / C-M--
(use-package default-text-scale
  :init (default-text-scale-mode))

;;; Key Bindings
(use-package general)

;; Development Modes

;;; ALL
;;;
;;; Projectile
(use-package projectile
  :config (progn
            (projectile-global-mode)
            (setq projectile-enable-caching nil
                  projectile-completion-system 'ivy)))
;;; Magit
(use-package magit
  :pin melpa-stable
  :config (progn
            (put 'magit-clean 'disabled nil)
            (add-hook 'magit-status-sections-hook 'magit-insert-worktrees)
            (setq magit-commit-show-diff nil)))
;;; EditorConfig
;;; Read files to set coding style options according to current prroject
(use-package editorconfig
  :config (editorconfig-mode 1))


;;; Other Modes
;;;
;;; Markdown Mode
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config (progn
            (add-hook 'markdown-mode-hook 'visual-line-mode)
            (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))))

(setq-default
 inhibit-splash-screen t
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 ;; no beeping or blinking please
 visible-bell nil
 blink-matching-paren nil ;; don't blink -- too distracting
 ;;confirm-kill-emacs 'yes-or-no-p
 )

;; TODO not sure if I need these yet
(setq dired-dwim-target t
      dired-recursive-deletes t
      dired-use-ls-dired nil
      delete-by-moving-to-trash t)
;; TODO: do I want emmet mode?
(use-package emmet-mode
  :config (progn
            (setq emmet-move-cursor-between-quotes t)
            (add-hook 'css-mode-hook  'emmet-mode)))

;; Theme
;;
(use-package doom-themes
  :init (load-theme 'doom-molokai t)
  :config
  (progn
    (doom-themes-org-config)))
;; Default Font
(set-face-attribute 'default nil
                    :family "Fira Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(global-display-line-numbers-mode t)
 '(initial-buffer-choice t)
 '(package-selected-packages (quote (which-key doom-themes editorconfig use-package)))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(vc-follow-symlinks t)
 '(version-control t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
