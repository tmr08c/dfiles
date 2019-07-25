;;; osx.el -- macOS specific configuration

;;; Commentary:

;;; Code:


(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-fontset-font t 'unicode "Apple Color Emoji" frame 'prepend)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-hook 'after-load-theme-hook
          (lambda ()
            (let ((bg (frame-parameter nil 'background-mode)))
              (set-frame-parameter nil 'ns-appearance bg)
              (setcdr (assq 'ns-appearance default-frame-alist) bg))))

(use-package reveal-in-osx-finder
  :commands reveal-in-osx-finder)

(use-package pbcopy
  :unless (display-graphic-p)
  :init (turn-on-pbcopy))

;; Enable built-in trash support via finder API if available (only on Emacs
;; Mac Port)
(when (boundp 'mac-system-move-file-to-trash-use-finder)
  (setq mac-system-move-file-to-trash-use-finder t))

(use-package osx-trash
  :defer 5
  :unless (boundp 'mac-system-move-file-to-trash-use-finder)
  :init (osx-trash-setup))

(when (display-graphic-p)
  ;; Treat command as super
  (setq mac-command-key-is-meta nil
        mac-command-modifier 'super
        mac-option-key-is-meta 't
        mac-option-modifier 'meta)

  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "s-s") (lambda ()
                                (interactive)
                                (call-interactively (key-binding "\C-x\C-s")))))


(provide 'osx)

;;; osx.el ends here
