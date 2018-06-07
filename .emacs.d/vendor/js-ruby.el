;;; js-ruby.el --- Part of my Emacs configuration

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :ensure nil
  :ensure-system-package
  ((ruby-lint   . "gem install ruby-lint")
   (ripper-tags . "gem install ripper-tags")
   (pry . "gem install pry"))
  :hook (ruby-mode . flycheck-mode)
  :custom
  (ruby-insert-encoding-magic-comment nil)
  (ruby-align-to-stmt-keywords '(if while unless until begin case for def))
  :general
  (space-leader-def
    :keymaps 'ruby-mode-map
    "m" '(:ignore t :which-key "Ruby")
    "m t" '(:ignore t :which-key "Tests")))

(use-package inf-ruby
  :requires ruby-mode
  :hook (after-init . inf-ruby-switch-setup))


(use-package rspec-mode
  :hook (ruby-mode . rspec-mode)
  :custom
  (compilation-scroll-output t)
  :general
  (space-leader-def 'normal ruby-mode-map
    "m t a" '(rspec-verify-all :which-key "run all tests")
    "m t b" '(rspec-verify :which-key "run tests in buffer")
    "m t e" '(rspec-toggle-example-pendingness :which-key "toggle test pending")
    "m t t" '(rspec-verify-single :which-key "run focus test")
    "m t l" '(rspec-run-last-failed :which-key "rerun failed tests")
    "m t r" '(rspec-rerun :which-key "rerun last tests")))

(use-package rubocop
  :requires ruby-mode
  :ensure-system-package
  (rubocop . "gem install rubocop")
  :hook (ruby-mode . rubocop-mode))

(use-package rbenv
  :requires ruby-mode
  :hook (ruby-mode . global-rbenv-mode))

(use-package yard-mode
  :requires ruby-mode
  :hook (ruby-mode . yard-mode))

(use-package ruby-hash-syntax
  :requires ruby-mode
  :general
  (space-leader-def 'normal ruby-mode-map
    "m f h" '(ruby-hash-syntax-toggle :which-key "toggle hash syntax")))

(use-package projectile-rails
  :requires projectile
  :hook (projectile-mode . projectile-rails-on))

(provide 'js-ruby)

;;; js-ruby.el ends here
