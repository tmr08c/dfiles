;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; Ensure Emacs is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

(defvar js|config-file
  (expand-file-name "config.el" user-emacs-directory)
  "The file path of your literate config file.")
;; (defvar +literate-config-file
;;   (expand-file-name "config.org" user-emacs-directory)
;;   "The file path of your literate config file.")

;; (defvar +literate-config-dest-file
;;   (expand-file-name "config.el" user-emacs-directory)
;;   "The file path that `+literate-config-file' will be tangled to, then
;; byte-compiled from.")

;; (defun +literate-compile (&optional load)
;;   "Tangles & compiles `+literate-config-file' if it has changed. If LOAD is
;; non-nil, load it too!"
;;   (let ((org +literate-config-file)
;;         (el  +literate-config-dest-file))
;;     (when (file-newer-than-file-p org el)
;;       (message "Compiling your literate config...")

;;       ;; We tangle in a separate, blank process because loading it here would
;;       ;; load all of :lang org (very expensive!). We only need ob-tangle.
;;       (or (zerop (call-process
;;                   "emacs" nil nil nil
;;                   "-q" "--batch" "-l" "ob-tangle" "--eval"
;;                   (format "(org-babel-tangle-file \"%s\" \"%s\")"
;;                           org el)))
;;           (warn "There was a problem tangling your literate config!"))

;;       (message "Done!"))))

;; (+literate-compile)

(when (file-readable-p (concat user-emacs-directory "config.el"))
  (load-file (concat user-emacs-directory "config.el")))

;;; This is the actual configuration file.
;;; It is omitted if it doesn't exist so emacs won't refuse to launch.
;; (when (file-readable-p (concat user-emacs-directory "config.org"))
;;   (org-babel-load-file (concat user-emacs-directory "config.org")))

;;(provide 'init)
;;; init.el ends here
