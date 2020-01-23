
(use-package ruby-mode
  :ensure nil
  ;; :ensure-system-package
  ;; ((ruby-lint   . "gem install ruby-lint")
  ;;  (ripper-tags . "gem install ripper-tags")
  ;;  (pry . "gem install pry"))
  :hook (ruby-mode . flycheck-mode)
  :config
  (add-hook 'ruby-mode-hook
            '(lambda ()
               ;; (add-hook 'before-save-hook 'lsp-format-buffer) ; Wait for newer solargraph gem
               (setq evil-shift-width ruby-indent-level)))
  (setq ruby-insert-encoding-magic-comment nil
        ruby-align-to-stmt-keywords
        '(if while unless until begin case for def)))
(use-package bundler
  :hook (ruby-mode . bundler-mode))
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
          (compilation-filter . inf-ruby-auto-enter)))
;; (use-package company-inf-ruby
;;   :after inf-ruby
;;   :config
;;   (add-to-list 'company-backends 'company-inf-ruby))
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
             rspec-install-snippets
             rspec-dired-verify-single)
  :hook ((dired-mode . rspec-dired-mode)
         (ruby-mode . rspec-mode))
  :config
  (setq compilation-scroll-output 'first-error
        rspec-use-spring-when-possible nil
        rspec-autosave-buffer t)
  (add-hook 'rspec-compilation-mode-hook 'inf-ruby-auto-enter nil t)
  (with-eval-after-load 'yasnippet
    (rspec-install-snippets))
  ;; (with-eval-after-load 'smartparens
  ;;   (sp-with-modes 'ruby-mode
  ;;     (sp-local-pair
  ;;      "{" "}"
  ;;      :pre-handlers '(sp-ruby-pre-handler)
  ;;      :post-handlers '(sp-ruby-post-handler
  ;;                       (js|smartparens-pair-newline-and-indent "RET"))
  ;;      :suffix "")))
  )
;; (use-package rubocop
;;   :diminish
;;   ;; :ensure-system-package
;;   ;; (rubocop . "gem install rubocop")
;;   :hook (ruby-mode . rubocop-mode)
;;   ;; :config
;;   ;; (setq rubocop-autocorrect-on-save t)
;;   )
;; (use-package rubocopfmt
;;   :disabled
;;   :hook (ruby-mode . rubocopfmt-mode)
;;   ;; :config
;;   ;; (setq rubocopfmt-rubocop-command "rubocop-daemon-wrapper")
;;   :config
;;   (add-hook 'ruby-mode-hook
;;             (lambda ()
;;               (add-hook 'before-save-hook 'rubocopfmt)))
;;   )
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
	     :after projectile
  :diminish
  :config
  (projectile-rails-global-mode))

;; (use-package ruby-test-mode
;;   :commands (ruby-test-run
;;              ruby-test-run-at-point
;;              ruby-test-toggle-between-implementation-and-specification))
;; (use-package minitest
;;   :commands (minitest-verify-all
;;              minitest-verify
;;              minitest-rerun
;;              minitest-verify-single))
(use-package feature-mode
  :mode (("\\.feature\\'" . feature-mode)))

(provide 'ruby)
