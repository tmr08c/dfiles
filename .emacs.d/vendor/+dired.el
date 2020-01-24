;;; +dired.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Show git info in dired
(use-package dired-git-info
  :disabled
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Colourful dired
(use-package diredfl
  :init (diredfl-global-mode 1))

;; Shows icons
(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (with-no-warnings
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files in a dired buffer."
      (when dired-subdir-alist
        (let ((inhibit-read-only t))
          (save-excursion
            ;; TRICK: Use TAB to align icons
            (setq-local tab-width 1)
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (insert " ")
                (let ((file (dired-get-filename 'verbatim t)))
                  (unless (member file '("." ".."))
                    (let ((filename (dired-get-filename nil t)))
                      (if (file-directory-p filename)
                          (insert (all-the-icons-icon-for-dir filename nil ""))
                        (insert (all-the-icons-icon-for-file file :v-adjust -0.05))))
                    ;; Align and keep one space for refeshing after some operations
                    (insert "\t "))))
              (forward-line 1))))))
    (advice-add #'all-the-icons-dired--display
                :override #'my-all-the-icons-dired--display)))

(provide '+dired)
