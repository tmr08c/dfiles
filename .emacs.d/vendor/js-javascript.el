;;; js-javascript.el -- Part of my Emacs config

;;; Commentary:

;; This file contains the configuration for all JavaScript projects

;;; Code:

(use-package js2-mode
  :disabled
  :mode "\\.js\\'"
  :ensure-system-package
  (eslint_d . "npm install -g eslint_d")
  ;; :bind
  ;; (:map js2-mode-map
  ;;       ("," . self-with-space)
  ;;       ("=" . pad-equals)
  ;;       (":" . self-with-space))
  :interpreter
  ("node" . js2-mode)
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :custom
  (js2-mode-show-strict-warnings nil)
  (js2-highlight-level 3)
  :config
  (defvaralias 'js-switch-indent-offset 'js2-basic-offset)
  (setenv "NODE_NO_READLINE" "1")
  (after flycheck
         (setq flycheck-javascript-eslint-executable "eslint_d")))

(use-package tern
  :disabled
  :ensure-system-package (tern . "npm i -g tern")
  :requires js2-mode
  :hook
  (js2-mode . tern-mode))

(use-package company-tern
  :requires (company tern)
  :config
  (add-to-list 'company-backends #'company-tern))

(use-package nodejs-repl
  :ensure-system-package node
  :defer t)

(use-package ember-mode
  :disabled
  :ensure-system-package (ember . "npm i -g ember-cli"))

;;; React
(use-package rjsx-mode
  :requires js2-mode
  :config
  (bind-key "=" #'pad-equals rjsx-mode-map
            (not (memq (js2-node-type (js2-node-at-point))
                       (list rjsx-JSX rjsx-JSX-ATTR rjsx-JSX-IDENT rjsx-JSX-MEMBER)))))

(provide 'js-javascript)

;;; js-javascript.el ends here
