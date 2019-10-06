;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; (load! "+bindings")

(setq doom-font (font-spec :family "Cascadia Code")
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-theme 'doom-one)

;; (let ((emoji-font-face
;;        (if IS-MAC "Apple Color Emoji" "Twitter Color Emoji"))
;;       (font-size
;;        (if IS-MAC 13 15)))
;;   (setq doom-unicode-font (font-spec :name emoji-font-face :size font-size)
;;         doom-font (font-spec :family "Fira Mono" :size font-size)))

;; (when IS-LINUX
;;   (map!
;;    ;; Use Super-S to save like I am on macOS
;;    :n "s-s" (λ! (call-interactively (key-binding "\C-x\C-s")))))

(def-package! aggressive-indent
  :hook ((emacs-lisp-mode css-mode lisp-mode) . aggressive-indent-mode))

;; (after! doom-themes
;;   (remove-hook! 'doom-load-theme-hook 'doom-themes-neotree-config))

;; (after! go-mode
;;   (add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on save

;;   (defun my-go-mode-hook-fn ()
;;     (setq tab-width 2
;;           indent-tabs-mode 1)
;;     (flycheck-gometalinter-setup)
;;     (flycheck-mode 1))
;;   (add-hook 'go-mode-hook #'my-go-mode-hook-fn))

(after! company
  (setq company-idle-delay 0.6
        company-minimum-prefix-length 2))

(after! counsel
  (setq counsel-rg-base-command "rg -zS -M 150 --no-heading --line-number --color never %s ."))

(after! ivy
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

;; (after! projectile
;;   (setq projectile-globally-ignored-file-suffixes (append (list ".elc"))
;;         projectile-globally-ignored-directories (append (list "node_modules"))
;;         projectile-track-known-projects-automatically nil
;;         counsel-projectile-sort-projects t
;;         projectile-ignored-projects nil))

;; (after! swiper
;;   (map!
;;    :n "C-s" 'swiper))

(after! which-key
  (setq which-key-idle-delay 0.8))
