;;; js-java.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eclimd
  :disabled
  :hook (java-mode . eclim-mode))

(use-package company-emacs-eclim
  :disabled
  :hook company
  :hook (java-mode . company-emacs-eclim-setup))


(provide 'js-java)

;;; js-java.el ends here
