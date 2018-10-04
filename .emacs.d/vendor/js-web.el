;;; js-web.el --- Part of my Emacs config -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains all configuration for front-end Web
;; development (things like HTML, CSS, JSON)

;;; Code:

(use-package web-mode
  :mode
  (("\\.html\\'"       . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.php\\'"        . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.inky-erb\\'"   . web-mode)
   ("\\.inky\\'"       . web-mode)
   ("\\.hbs\\'"        . web-mode))
  ;; :bind
  ;; (:map web-mode-map
  ;;       ("," . self-with-space)
  ;;       ("<C-return>" . html-newline-dwim))
  :config
  (add-hook 'web-mode-hook #'turn-off-smartparens-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t))
(use-package company-web
  :hook web-mode
  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package css-mode
  :mode "\\.css\\.erb\\'"
  ;; :bind
  ;; (:map css-mode-map
  ;;       ("," . self-with-space)
  ;;       ("{" . open-brackets-newline-and-indent))
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :mode "\\.scss$")

(use-package counsel-css
  :hook (css-mode . counsel-css-imenu-setup))

(use-package web-beautify
  :hook web-mode)

(provide 'js-web)
;;; js-web.el ends here
