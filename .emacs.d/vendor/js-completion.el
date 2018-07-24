;;; js-completion.el -- Part of my Emacs configuration

;;; Commentary:

;;; Code:


;;; Company
;;; Auto-completion framework for most modes
(use-package company
  :delight
  :defer 2
  :custom
  (company-tooltip-limit 20)
  (company-idle-delay 0.3)
  (company-echo-delay 0) ; remove annoying blinking
  (company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  ;; (company-frontends'(company-echo-metadata-frontend
  ;;                     company-pseudo-tooltip-unless-just-one-frontend-with-delay
  ;;                     company-preview-frontend))
  :hook
  (after-init . global-company-mode))

(use-package company-box
  ;; :disabled
  :defer 5
  :load-path "vendor/company-box/"
  :hook (company-mode . company-box-mode))

(use-package company-statistics
  :after company
  :hook (company-mode . company-statistics-mode))

(use-package company-quickhelp
  ;; :disabled
  :custom
  (company-quickhelp-delay 0.1)
  :hook (company-mode . company-quickhelp-mode))

(use-package company-flx
  :disabled
  :hook (company-mode . company-flx-mode))

(use-package company-posframe
  :disabled
  :delight
  :after company-mode
  :config
  (company-posframe-mode 1))


;;; General
(use-package company-emoji
  :after company
  :config (add-to-list 'company-backends 'company-emoji))


;;; C/C++
(use-package company-irony-c-headers
  :after (company company-irony)
  :config (add-to-list 'company-backends 'company-irony-c-headers))

(use-package company-irony
  :hook irony-mode
  :custom (company-irony-ignore-case 'smart)
  :config (add-to-list 'company-backends 'company-irony))


;;; Python
(use-package company-anaconda
  :requires company
  :hook (python-mode . anaconda-mode)
  :config (add-to-list 'company-backends 'company-anaconda))


;;; Golang
(use-package company-go
  ;; :requires (company go-mode)
  ;; :after company
  :hook go-mode
  :custom
  (company-go-show-annotation t)
  :config (add-to-list 'company-backends 'company-go))


;;; Language Server Mode
(use-package lsp-mode
  :disabled
  :hook prog-mode
  :custom
  (lsp-message-project-root-warning t))

(use-package lsp-ui
  :disabled
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :disabled
  :after (company lsp-mode)
  :custom
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  :config
  (push 'company-lsp company-backends))


(custom-set-faces
 '(company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

(provide 'js-completion)
;;; js-completion.el ends here
