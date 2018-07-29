;;; js-scala.el -- Part of my emacs configuration

;;; Commentary:

;;; Code:

;; Scala
(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode))

(use-package ensime
  :hook (scala-mode . ensime-mode))

(use-package sbt-mode
  :hook (scala-mode . sbt-mode))


(provide 'js-scala)

;;; js-scala.el ends here
