;;; deprecate.el --- Packages I want to stop using -*- lexical-binding: t; -*-

;;; Commentary:
;;; These are packages I want to stop using and have to learn the alternatives for.

;;; Code:

(use-package doom-modeline ; TODO install spaceline all-the-icons
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-lsp t))

(use-package neotree ; Try to use dired instead
  :commands (neotree-toggle neotree-projectile-action)
  :config
  (js|neotree-keybindings)
  (setq neo-create-file-auto-open t
        neo-modern-sidebar t
        neo-point-auto-indent nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-fixed-size nil
        neo-window-width 28
        neo-show-hidden-files t
        neo-keymap-style 'concise))

(provide 'deprecate)
