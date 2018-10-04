;;; js-ruby.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :ensure nil
  :ensure-system-package
  ((ruby-lint   . "gem install ruby-lint")
   (ripper-tags . "gem install ripper-tags")
   (pry . "gem install pry"))
  :hook (ruby-mode . flycheck-mode)
  :config
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (setq evil-shift-width ruby-indent-level)))
  (keymap-for-mode 'ruby-mode
                   "T" '(:ignore t :which-key "toggle")
                   "T'" 'ruby-toggle-string-quotes
                   "T{" 'ruby-toggle-block)
  :custom
  (ruby-insert-encoding-magic-comment nil)
  (ruby-align-to-stmt-keywords
   '(if while unless until begin case for def)))

(use-package bundler
  :hook (ruby-mode . bundler-mode)
  :config
  (keymap-for-mode 'ruby-mode
                   "b" '(:ignore t :which-key "bundle")
                   "bc" 'bundle-check
                   "bi" 'bundle-install
                   "bs" 'bundle-console
                   "bu" 'bundle-update
                   "bx" 'bundle-exec
                   "bo" 'bundle-open))

(use-package inf-ruby
  :custom
  (inf-ruby-console-environment "development")
  :hook
  (after-init . inf-ruby-switch-setup)
  :config
  (keymap-for-mode 'ruby-mode
                   "s" '(:ignore t :which-key "repl")
                   "sb" 'ruby-send-buffer
                   "sB" 'ruby-send-buffer-and-go
                   "sf" 'ruby-send-definition
                   "sF" 'ruby-send-definition-and-go
                   "sl" 'ruby-send-line
                   "sL" 'ruby-send-line-and-go
                   "sr" 'ruby-send-region
                   "sR" 'ruby-send-region-and-go
                   "ss" 'ruby-switch-to-inf))

(use-package company-inf-ruby
  :after inf-ruby
  :config
  (add-to-list 'company-backends 'company-inf-ruby))

;; Not available yet on MELPA
;; (use-package lsp-ruby
;;   :requires lsp-mode
;;   :hook (ruby-mode . lsp-ruby-enable))

;; (use-package robe
;;   :disabled
;;   :hook (ruby-mode . robe-mode)
;;   :config (add-to-list 'company-backends 'company-robe))

(use-package rspec-mode
  :hook (ruby-mode . rspec-mode)
  :custom
  (compilation-scroll-output 'first-error)
  (rspec-autosave-buffer t)
  :config
  (add-hook 'rspec-compilation-mode-hook 'inf-ruby-auto-enter nil t)
  (with-eval-after-load 'smartparens
    (sp-with-modes 'ruby-mode
      (sp-local-pair
       "{" "}"
       :pre-handlers '(sp-ruby-pre-handler)
       :post-handlers '(sp-ruby-post-handler
                        (js|smartparens-pair-newline-and-indent "RET"))
       :suffix "")))
  (keymap-for-mode 'ruby-mode
                   "t" '(:ignore t :which-key "test")
                   "ta"    'rspec-verify-all
                   "tb"    'rspec-verify
                   "tc"    'rspec-verify-continue
                   "td"    'ruby/rspec-verify-directory
                   "te"    'rspec-toggle-example-pendingness
                   "tf"    'rspec-verify-method
                   "tl"    'rspec-run-last-failed
                   "tm"    'rspec-verify-matching
                   "tr"    'rspec-rerun
                   "tt"    'rspec-verify-single
                   "t~"    'rspec-toggle-spec-and-target-find-example
                   "t TAB" 'rspec-toggle-spec-and-target))

(use-package rubocop
  :ensure-system-package
  (rubocop . "gem install rubocop")
  :hook (ruby-mode . rubocop-mode)
  :config
  (keymap-for-mode 'ruby-mode
                   "rr" '(:ignore t :which-key "Rubocop")
                   "rrd" 'rubocop-check-directory
                   "rrD" 'rubocop-autocorrect-directory
                   "rrf" 'rubocop-check-current-file
                   "rrF" 'rubocop-autocorrect-current-file
                   "rrp" 'rubocop-check-project
                   "rrP" 'rubocop-autocorrect-project))

(use-package rbenv
  :hook (ruby-mode . global-rbenv-mode))

(use-package yard-mode
  :hook (ruby-mode . yard-mode))

(use-package ruby-hash-syntax
  :requires ruby-mode
  :config
  (keymap-for-mode 'ruby-mode
                   "fh" 'ruby-hash-syntax-toggle))

(use-package projectile-rails
  :requires projectile
  :hook (projectile-mode . projectile-rails-on))

(provide 'js-ruby)

;;; js-ruby.el ends here
