;;; js-altmodes.el -- Part of my Emacs configuration

;;; Commentary:
;;; This file contains all the one-off modes and extra things like highlighting
;;; that do not warrant their own file or to clutter the main configuration.

;;; Code:


;; Markdown Mode
(use-package markdown-mode
  :mode "\\.md$"
  :hook (markdown-mode . flyspell-mode))

;; CSV
(use-package csv-mode
  :mode "\\.csv$"
  :config
  (defun csv-align-visible ()
    "Align only visible entries in csv-mode."
    (interactive)
    (csv-align-fields nil (window-start) (window-end)))

  ;; C-c C-a is already bound to align all fields, but can be too slow.
  :bind (:map csv-mode-map
              ("C-c C-w" . 'csv-align-visible)))

;; JSON Formatter
(use-package json-mode
  :custom
  (js-indent-level 2)
  :mode ("\\.json$"
         "\\.jshintrc$"))

;;; Dockerfile
(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

;;; YAML mode
(use-package yaml-mode
  :mode "\\.ya?ml\'")

;;; Git Attributes
(use-package gitattributes-mode
  :disabled
  :mode ("/\\.gitattributes\\'"
         "/info/attributes\\'"
         "/git/attributes\\'"))

(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'"
         "/\\.git/config\\'"
         "/modules/.*/config\\'"
         "/git/config\\'"
         "/\\.gitmodules\\'"
         "/etc/gitconfig\\'"))

(use-package gitignore-mode
  :mode ("/\\.gitignore\\'"
         "/info/exclude\\'"
         "/git/ignore\\'"))

(provide 'js-altmodes)

;;; js-altmodes.el ends here
