(setq package-check-signature nil)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;;(setq use-package-verbose t)
;;(setq use-package-always-ensure t)
;;(use-package auto-compile
;;  :config (auto-compile-on-load-mode))
;;(setq load-prefer-newer t)

;; Use spaces instead of Tabs
(setq-default indent-tabs-mode nil)

(setq user-full-name "Justin Smestad"
      user-mail-address "justin.smestad@gmail.com")

;; EditorConfig
;; Read files to set coding style options according to current prroject
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Rainbow Mode
;; Show HEX colors inline
;; (use-package rainbow-mode
;;   :ensure t)

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-molokai t)
  :config
  (progn
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)))

;; Enable which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
    (which-key-setup-side-window-right-bottom)
    (setq which-key-sort-order 'which-key-key-order-alpha
          which-key-side-window-max-width 0.33
          which-key-idle-delay 0.05)
  :diminish which-key-mode)

;; Detect underlying OS
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(setq-default
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil
 blink-matching-paren nil ; don't blink -- too distracting
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(package-selected-packages (quote (which-key use-package doom-themes)))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Mono" :foundry "CTDB" :slant normal :weight normal :height 113 :width normal)))))
