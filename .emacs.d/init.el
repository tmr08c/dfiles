;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;; Add load path for vendor directory
(add-to-list 'load-path (concat user-emacs-directory "vendor/"))

;;; Get package repos configured
(require 'package)
(customize-set-variable
 'package-archives
 '(("melpa" . "https://melpa.org/packages/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("org" . "https://orgmode.org/elpa/")))
(unless package--initialized
  (package-initialize))

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-compute-statistics t)

(eval-when-compile
  (require 'use-package))

;;; This is the actual configuration file.
;;; It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p (concat user-emacs-directory "config.org"))
  (org-babel-load-file (concat user-emacs-directory "config.org")))

(provide 'init)
;;; init.el ends here
