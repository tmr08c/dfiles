;;; js-vc.el --- Part of my Emacs configuration

;;; Commentary:

;;; Code:

;;; Magit
(use-package magit
  :defer 5)
(use-package magithub
  :disabled
  :after magit
  :config
  (magithub-feature-autoinject t))
;; May not be needed:
;; :custom
;; (magit-commit-show-diff nil)
;; :hook (magit-status-sections . magit-insert-worktrees)
;; :config
;; (put 'magit-clean 'disabled nil))

(use-package evil-magit
  :after (magit evil))

(provide 'js-vc)
;;; js-vc.el ends here
