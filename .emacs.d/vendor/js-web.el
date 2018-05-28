;;; js-web.el --- Part of my Emacs config

;;; Commentary:

;; This file contains all configuration for front-end Web
;; development (things like HTML, CSS, JSON)

;;; Code:

(use-package web-mode
  :mode
  (("\\.erb\\'"        . web-mode)
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
  :custom
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t))
(use-package company-web
  :requires (web-mode company)
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

(use-package counsel-css
  :hook
  (css-mode . counsel-css-imenu-setup))


(provide 'js-web)

;;; js-web.el ends here
