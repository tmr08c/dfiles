(use-package add-node-modules-path
  :hook ((js2-mode js-mode json-mode typescript-mode elm-mode) . add-node-modules-path))
(use-package js2-mode
  :mode "\\.m?js\\'"
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :config
  (setq-default js-switch-indent-offset 2
                js-indent-level 2)
  (setenv "NODE_NO_READLINE" "1"))
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2
        typescript-expr-indent-offset 2))

(use-package company-web
  :requires company
  :hook (web-mode . (lambda ()
                      (setq-local company-backends '(company-web-html company-css company-yasnippet)))))
(use-package web-mode
  :hook (html-mode . web-mode)
  :diminish
  :config
  (setq   web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-block-padding 0
          web-mode-enable-block-face t
          web-mode-enable-current-column-highlight t
          web-mode-auto-close-style 2
          web-mode-enable-html-entities-fontification t
          web-mode-enable-current-element-highlight t
          web-mode-enable-auto-quoting t
          web-mode-enable-auto-pairing t)
  (add-hook 'web-mode-hook #'turn-off-smartparens-mode))
(use-package emmet-mode
  :diminish
  :hook ((css-mode web-mode) . emmet-mode))

;; NOTE does not work yet due to TRAMP stuff
;; (use-package prettier
;;   :quelpa
;;   (prettier :fetcher github :repo "jscheid/prettier.el")
;;   :init (global-prettier-mode 1))

(use-package css-mode
  :mode "\\.css$"
  :hook (css-mode . (lambda ()
                      (setq-local company-backends '(company-css company-yasnippet))))
  :custom
  (css-indent-offset 2))
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
