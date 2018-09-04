;;; js-elixir.el --- Part of my emacs configuration
;;; Commentary:

;;; Code:

(use-package elixir-mode
  :commands elixir-mode
  :mode "\\.exs?")

(use-package alchemist
  :commands alchemist-mode
  :hook (elixir-mode . alchemist-mode))

(use-package flycheck-mix
  :hook (elixir-mode . flycheck-mix-setup))


(provide 'js-elixir)
;;; js-elixir.el ends here
