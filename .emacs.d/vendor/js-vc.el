;;; js-vc.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Magit
(use-package magit
  :commands
  (magit-status magit-log-all-branches))
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
  :requires (magit evil))

(use-package gist
  :commands (gist-buffer
             gist-buffer-private
             gist-region
             gist-region-private)
  :config
  (evil-set-initial-state 'gist-list-mode 'normal)
  (defun +gist*list-render (orig-fn &rest args)
    (funcall orig-fn (car args) t)
    (unless (cadr args)
      (pop-to-buffer (current-buffer))))
  (advice-add #'gist-list-render :around #'+gist*list-render)
  (global-keymap
   "gg" '(:ignore t :which-key "github gist")
   "ggb" 'gist-buffer
   "ggB" 'gist-buffer-private
   ;; "ggl" 'gist-list
   "ggr" 'gist-region
   "ggR" 'gist-region-private))

(use-package github-clone
  :commands (github-clone
             github-clone-add-existing-remote
             github-clone-fork-remote
             github-clone-add-source-remote)
  :init
  (global-keymap
   "ghc" '(:ignore t :which-key "clone")
   "ghcc" 'github-clone
   "ghcr" 'github-clone-add-existing-remote
   "ghcf" 'github-clone-fork-remote
   "ghcu" 'github-clone-add-source-remote))

(provide 'js-vc)
;;; js-vc.el ends here
