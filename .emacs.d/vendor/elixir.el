;; Erlang / Elixir
(use-package erlang)
(use-package elixir-mode
  :config
  (progn
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'lsp-format-buffer)))
    ;; (add-hook 'elixir-format-hook (lambda ()
    ;;                                 (if (projectile-project-p)
    ;;                                     (setq elixir-format-arguments
    ;;                                           (list "--dot-formatter"
    ;;                                                 (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
    ;;                                   (setq elixir-format-arguments nil))))
    ))
(use-package exunit
  :commands (exunit-verify-all
             exunit-verify-all-in-umbrella
             exunit-verify
             exunit-verify-single
             exunit-rerun))
(use-package alchemist
  :disabled
  :hook (elixir-mode . alchemist-mode)
  :config
  (setq alchemist-project-compile-when-needed t
        alchemist-test-status-modeline t
        alchemist-test-truncate-lines nil)
  (dolist (mode (list alchemist-compile-mode-map
                      alchemist-eval-mode-map
                      alchemist-execute-mode-map
                      alchemist-message-mode-map
                      alchemist-help-minor-mode-map
                      alchemist-mix-mode-map
                      alchemist-macroexpand-mode-map
                      alchemist-refcard-mode-map
                      alchemist-test-report-mode-map))
    (evil-define-key 'normal mode
      (kbd "q") 'quit-window)))
(use-package alchemist-company
  :disabled
  :ensure nil
  :hook (elixir-mode . (lambda ()
                         (setq-local company-backends '(alchemist-company company-yasnippet)))))
(use-package flycheck-credo
  :hook (elixir-mode . flycheck-credo-setup))
(use-package flycheck-mix
  :hook (elixir-mode . flycheck-mix-setup))
;; (use-package flycheck-mix
;;   :commands (flycheck-mix-setup)
;;   :init
;;   (progn
;;     (add-to-list 'safe-local-variable-values
;;                  (cons 'elixir-enable-compilation-checking nil))
;;     (add-to-list 'safe-local-variable-values
;;                  (cons 'elixir-enable-compilation-checking t))
;;     (add-hook 'elixir-mode-local-vars-hook
;;               'spacemacs//elixir-enable-compilation-checking)))

(provide 'elixir)
