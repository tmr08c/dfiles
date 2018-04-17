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

;;(setq use-package-verbose t)
;;(setq use-package-always-ensure t)
;;(use-package auto-compile
;;  :config (auto-compile-on-load-mode))
;;(setq load-prefer-newer t)


(setq user-full-name "Justin Smestad"
      user-mail-address "justin.smestad@gmail.com")

;; EditorConfig
;; Read files to set coding style options according to current prroject
(use-package editorconfig
  :config (editorconfig-mode 1))

;; Rainbow Mode
;; Show HEX colors inline
;; (use-package rainbow-mode
;;   :ensure t)

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-molokai t)
  :config
  (progn
    (doom-themes-org-config)))

;; Enable which-key
(use-package which-key
  :delight
  :init (which-key-mode)
  :config
    (which-key-setup-side-window-right-bottom)
    (setq which-key-sort-order 'which-key-key-order-alpha
          which-key-side-window-max-width 0.33
          which-key-idle-delay 0.05))

(use-package default-text-scale
  :init (default-text-scale-mode))

;; Detect underlying OS
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(setq-default
 inhibit-splash-screen t
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 ;; no beeping or blinking please
 visible-bell nil
 blink-matching-paren nil ; don't blink -- too distracting
 confirm-kill-emacs 'yes-or-no-p)

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
 '(show-paren-mode t))

