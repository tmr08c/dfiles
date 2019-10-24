
;; Emacs Lisp (elisp)
(use-package ielm)
(use-package eros
  :commands (eros-eval-defun eros-eval-last-sexp eros-mode))
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode)
  :commands highlight-quoted-mode)
(use-package macrostep
  :mode ("\\*.el\\'" . emacs-lisp-mode))
(use-package overseer)
(use-package elisp-def
  :disabled)
(use-package elisp-demos
  :disabled)
(use-package flycheck-cask
  :hook (emacs-lisp-mode . flycheck-cask-setup))

(provide 'lisp)
