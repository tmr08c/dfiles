;;; js-completion.el -- Part of my Emacs configuration

;;; Commentary:

;;; Code:


;;; Company
;;; Auto-completion framework for most modes
(use-package company
  :delight
  :custom
  (company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-echo-delay 0) ; remove annoying blinking
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 20)
  (company-backends '(
                      company-nxml
                      company-css
                      company-files
                      company-c-headers
                      (company-dabbrev-code company-etags company-capf company-keywords)
                      company-dabbrev))
  ;; (company-frontends '(company-tng-frontend))
  ;; (company-frontends '(company-echo-metadata-frontend
  ;;                      company-pseudo-tooltip-unless-just-one-frontend-with-delay
  ;;                      company-preview-frontend))
  :hook
  (after-init . global-company-mode)
  ;; :config (add-to-list 'company-frontends 'company-tng-frontend)
  )

(use-package company-box
  :disabled
  ;; :defer 5
  :load-path "vendor/company-box/"
  :hook (company-mode . company-box-mode)
  ;; :bind
  ;; (:map company-active-map
  ;;       ("TAB" . company-complete-common)
  ;;       ("<tab>" . company-complete-common)
  ;;       ("RET" . company-complete-selection)
  ;;       ([return] . company-complete-selection)
  ;;       ("C-/" . company-search-candidates)
  ;;       ("C-M-/" . company-filter-candidates)
  ;;       ("C-d" . company-show-doc-buffer)
  ;;       ("C-j" . company-select-next)
  ;;       ("C-k" . company-select-previous)
  ;;       ("C-l" . company-complete-selection))
  )

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-quickhelp
  :custom
  (company-quickhelp-delay 0.1)
  :hook (company-mode . company-quickhelp-mode)
  :general
  (general-def 'insert company-quickhelp-mode-map
    "C-k" 'company-select-previous))

(use-package company-flx
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

(use-package counsel-etags)

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
  :hook (go-mode . (lambda ()
                     (set (make-local-variable 'company-backends) '(company-go))
                     (company-mode)))
  :custom
  (company-go-show-annotation t)
  :config (add-to-list 'company-backends 'company-go))


;;; Language Server Mode
(use-package eglot
  :disabled ;; Works butjust not as good as company-go
  :after company
  :config
  (progn
    (add-to-list
     'eglot-server-programs
     '(go-mode . ("go-langserver" "-gocodecompletion")))))

(use-package lsp-mode
  :disabled ;; TODO: replace with eglot
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
