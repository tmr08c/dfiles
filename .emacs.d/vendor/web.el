;;; web.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package add-node-modules-path
  :disabled
  :hook ((js2-mode js-mode json-mode typescript-mode elm-mode) . add-node-modules-path))

(use-package js2-mode
  :mode "\\.m?js\\'"
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :config
  (setq js-chain-indent t
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1)
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  (setq-default js-switch-indent-offset 2
                js-indent-level 2)
  (setenv "NODE_NO_READLINE" "1"))

;; (use-package rjsx-mode) ;; React

;; (use-package js2-refactor)
;; (use-package npm-mode)
(use-package eslintd-fix
  :hook (js2-mode . eslintd-fix-mode))

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (setq typescript-indent-level 2
        typescript-expr-indent-offset 2))

;; (use-package company-web
;;   :requires company
;;   :hook (web-mode . (lambda ()
;;                       (setq-local company-backends '(company-web-html company-css company-yasnippet)))))

(use-package web-mode
  :mode "\\.\\(?:as\\(?:[cp]x\\)\\|blade\\.php\\|erb\\|hbs\\|j\\(?:inja\\|sp\\)\\|mustache\\|p?html?\\|svelte\\|t\\(?:pl\\.php\\|sx\\|wig\\)\\|vue\\)\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :mode "\\.hbs\\'"
  :diminish
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-comment-style 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 0
        web-mode-enable-block-face t
        web-mode-auto-close-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-html-entities-fontification t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight nil
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-enable-auto-pairing t)
  (add-hook 'web-mode-hook #'turn-off-smartparens-mode))

(use-package css-mode
  :mode "\\.css$"
  :config
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook #'rainbow-delimiters-mode))
(use-package scss-mode
  :mode "\\.scss$")
(use-package sass-mode
  :mode "\\.sass$")

(with-eval-after-load 'smartparens
  (sp-local-pair 'web-mode "<" ">")
  (sp-with-modes '(css-mode scss-mode)
    (sp-local-pair "/*" "*/"
                   :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))))

(provide 'web)
;;; web.el ends here
