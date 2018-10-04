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
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq evil-shift-width ruby-indent-level)))
  :custom
  (ruby-insert-encoding-magic-comment nil)
  (ruby-align-to-stmt-keywords '(if while unless until begin case for def)))

(use-package inf-ruby
  :custom
  (inf-ruby-console-environment "development")
  :hook
  (after-init . inf-ruby-switch-setup))

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
  (keymap-for-mode 'ruby-mode
                   "t" '(:ignore t :which-key "Tests")
                   "ta" '(rspec-verify-all :which-key "run all tests")
                   "tb" '(rspec-verify :which-key "run tests in buffer")
                   "te" '(rspec-toggle-example-pendingness :which-key "toggle test pending")
                   "tt" '(rspec-verify-single :which-key "run focus test")
                   "tl" '(rspec-run-last-failed :which-key "rerun failed tests")
                   "tr" '(rspec-rerun :which-key "rerun last tests")))

(use-package rubocop
  :ensure-system-package
  (rubocop . "gem install rubocop")
  :hook (ruby-mode . rubocop-mode))

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
