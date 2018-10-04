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

;; Editorconfig - Read files to set coding style options according to current project
(use-package editorconfig
  :disabled
  :config (editorconfig-mode 1))

(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (keymap-for-mode 'pdf-view
                   ;; Slicing image
                   "sm" 'pdf-view-set-slice-using-mouse
                   "sb" 'pdf-view-set-slice-from-bounding-box
                   "sr" 'pdf-view-reset-slice
                   ;; Annotations
                   "a" '(:ignore t :which-key "annotations")
                   "aD" 'pdf-annot-delete
                   "at"	'pdf-annot-attachment-dired
                   "ah"	'pdf-annot-add-highlight-markup-annotation
                   "al"	'pdf-annot-list-annotations
                   "am"	'pdf-annot-add-markup-annotation
                   "ao"	'pdf-annot-add-strikeout-markup-annotation
                   "as"	'pdf-annot-add-squiggly-markup-annotation
                   "at"	'pdf-annot-add-text-annotation
                   "au"	'pdf-annot-add-underline-markup-annotation
                   ;; Fit image to window
                   "f" '(:ignore t :which-key "fit")
                   "fw" 'pdf-view-fit-width-to-window
                   "fh" 'pdf-view-fit-height-to-window
                   "fp" 'pdf-view-fit-page-to-window
                   ;; Other
                   "s" '(:ignore t :which-key "slice/search")
                   "ss" 'pdf-occur
                   "p" 'pdf-misc-print-document
                   "O" 'pdf-outline
                   "n" 'pdf-view-midnight-minor-mode))

(use-package sql
  :defer t
  :custom
  (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  (sql-set-product-feature 'postgres :prompt-cont-regexp
                           "^[-[:alnum:]_]*[-(][#>] ")
  :config
  (progn
    (defun my-sql-login-hook ()
      "Custom SQL log-in behaviours. See `sql-login-hook'."
      ;; n.b. If you are looking for a response and need to parse the
      ;; response, use `sql-redirect-value' instead of `comint-send-string'.
      (when (eq sql-product 'postgres)
        (let ((proc (get-buffer-process (current-buffer))))
          ;; Output each query before executing it. (n.b. this also avoids
          ;; the psql prompt breaking the alignment of query results.)
          (comint-send-string proc "\\set ECHO queries\n"))))
    (add-hook 'sql-login-hook 'my-sql-login-hook)
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (toggle-truncate-lines t)))))

(use-package sql-indent
  :pin gnu
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlup-mode
  :hook (sql-mode sql-interactive-mode-hook))

(provide 'js-altmodes)

;;; js-altmodes.el ends here
