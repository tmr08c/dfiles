;;; js-editing.el --- Part of my Emacs setup

;;; Commentary:

;;; Code:

(use-package evil
  :init (evil-mode 1)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-shift-width 2))

(use-package evil-commentary
  :requires evil
  :init (evil-commentary-mode))

(use-package evil-surround
  :requires evil
  :init (global-evil-surround-mode))

(use-package evil-matchit
  :requires evil
  :init (global-evil-matchit-mode 1))

(use-package evil-escape
  :requires evil
  :custom
  (evil-escape-delay 0.2)
  :init (evil-escape-mode))


(provide 'js-editing)

;;; js-editing.el ends here
