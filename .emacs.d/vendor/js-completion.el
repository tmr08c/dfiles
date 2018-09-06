;;; js-completion.el -- Part of my Emacs configuration

;;; Commentary:

;;; Code:


;;; Company
;;; Auto-completion framework for most modes
(use-package company
  :delight
  :hook (after-init . global-company-mode)
  :custom
  ;; (company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-code-other-buffers t)
  (company-echo-delay 0) ; remove annoying blinking
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-require-match 'never)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 10)
  (company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode))
  (company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (company-transformers '(company-sort-by-occurrence))
  (company-backends '(company-capf company-dabbrev company-async-files)))

(use-package company-async-files
  :load-path "vendor/"
  :requires company)

(use-package company-box
  :hook (company-mode . company-box-mode)
  :load-path "vendor/company-box/")

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.1)
  :general
  (general-def 'insert company-quickhelp-mode-map
    "C-k" 'company-select-previous))

(use-package company-flx
  :hook (company-mode . company-flx-mode))

(use-package company-posframe
  :disabled
  :delight
  :hook (company-mode . company-posframe-mode))


;;; General
(use-package company-emoji
  :after company
  :config (add-to-list 'company-backends 'company-emoji))

;;; C/C++
(use-package company-irony-c-headers
  :after company-irony
  :hook (irony-mode . (lambda ()
                        (set (make-local-variable 'company-backends) '((company-irony-c-headers company-irony)))
                        (company-mode))))

(use-package company-irony
  :hook irony-mode
  :custom
  (company-irony-ignore-case 'smart))

;;; Python
(use-package company-anaconda
  :hook (python-mode . (lambda ()
                         (set (make-local-variable 'company-backends) '(company-anaconda))
                         (company-mode)
                         (anaconda-mode))))

;;; Golang
(use-package company-go
  :hook (go-mode . (lambda ()
                     (set (make-local-variable 'company-backends) '(company-go))
                     (company-mode)))
  :custom
  (company-go-show-annotation t))


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
