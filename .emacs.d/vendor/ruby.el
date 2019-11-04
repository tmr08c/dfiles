
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
  :custom
  (ruby-insert-encoding-magic-comment nil)
  (ruby-align-to-stmt-keywords
   '(if while unless until begin case for def)))
(use-package bundler
  :hook (ruby-mode . bundler-mode))
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter-hook . inf-ruby-auto-enter))
  :custom
  (inf-ruby-console-environment "development"))
(use-package company-inf-ruby
  :after inf-ruby
  :config
  (add-to-list 'company-backends 'company-inf-ruby))
(use-package rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :commands (rspec-verify-all
             rspec-rerun
             rspec-verify
             rspec-verify-continue
             rspec-run-last-failed
             rspec-toggle-spec-and-target
             rspec-toggle-spec-and-target-find-example
             rspec-verify-method
             rspec-verify-matching
             rspec-verify-single
             rspec-toggle-example-pendingness
             rspec-dired-verify
             rspec-dired-verify-single)
  ;; :hook (ruby-mode . rspec-mode)
  :config
  (setq compilation-scroll-output 'first-error
        rspec-autosave-buffer t)
  (add-hook 'rspec-compilation-mode-hook 'inf-ruby-auto-enter nil t)
  (with-eval-after-load 'smartparens
    (sp-with-modes 'ruby-mode
      (sp-local-pair
       "{" "}"
       :pre-handlers '(sp-ruby-pre-handler)
       :post-handlers '(sp-ruby-post-handler
                        (js|smartparens-pair-newline-and-indent "RET"))
       :suffix ""))))
(use-package rubocop
  :ensure-system-package
  (rubocop . "gem install rubocop")
  :hook (ruby-mode . rubocop-mode))
(use-package rubocopfmt
  :requires rubocop
  :hook (ruby-mode . rubocopfmt-mode))
(use-package rbenv
  :hook (ruby-mode . global-rbenv-mode))
(use-package yard-mode
  :hook (ruby-mode . yard-mode))
(use-package ruby-hash-syntax
  :requires ruby-mode
  :commands (ruby-hash-syntax-toggle))
(use-package ruby-refactor
  :commands (ruby-refactor-extract-to-method
             ruby-refactor-extract-local-variable
             ruby-refactor-extract-constant
             ruby-refactor-extract-to-let))
(use-package projectile-rails
  :requires projectile
  :hook (projectile-mode . projectile-rails-on))
;; (use-package ruby-test-mode
;;   :commands (ruby-test-run
;;              ruby-test-run-at-point
;;              ruby-test-toggle-between-implementation-and-specification))
(use-package minitest
  :commands (minitest-verify-all
             minitest-verify
             minitest-rerun
             minitest-verify-single))
(use-package feature-mode
  :mode (("\\.feature\\'" . feature-mode)))

(provide 'ruby)
