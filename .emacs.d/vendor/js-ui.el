;;; js-ui.el --- Part of my Emacs configuration

;;; Commentary:

;;; Code:

;; Use Github as the standard
;; ref http://hilton.org.uk/blog/source-code-line-length
(customize-set-variable 'fill-column 125)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'blink-matching-paran nil)
(customize-set-variable 'visible-bell nil)
(customize-set-variable 'ring-bell-function (lambda ()
                                              (invert-face 'mode-line)
                                              (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;;; Font
(set-face-attribute 'default nil
                    :family "Fira Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Appearance
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (menu-bar-mode 0)
      (horizontal-scroll-bar-mode 0)

      ;; Theme Emacs for dark color scheme
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))

      (add-hook 'after-init-hook 'set-frame-size-according-to-resolution)
      (add-hook 'after-make-frame-functions 'set-frame-size-according-to-resolution)))


(defun set-frame-size-according-to-resolution (&rest frame)
  "Set FRAME height to screen height and width to half total."
  (if window-system
      (let ((f (if (car frame)
		               (car frame)
	               (selected-frame))))
        (progn
          (set-frame-height f (display-pixel-height) nil 'pixelwise)
          (set-frame-width f (/ (display-pixel-width) 2) nil 'pixelwise)
          (set-frame-position f 0 0)))))

(use-package all-the-icons)

;;; Theme
(use-package doom-themes
  :init (load-theme 'doom-molokai t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))


;; Modeline
;; (use-package shrink-path)
;; (use-package eldoc-eval)
(use-package doom-modeline
  ;; :requires (eldoc-eval shrink-path)
  ;; :load-path "vendor/"
  :hook (after-init . doom-modeline-init))
(use-package hide-mode-line
  :hook ((neotree-mode . hide-mode-line-mode)
         (completion-list-mode . hide-mode-line-mode)
         (completion-in-region-mode . hide-mode-line-mode)))


;;; Support Emojis in Emacs
(use-package emojify
  :defer 5
  :init
  (progn
    (setq emojify-display-style 'unicode)
    (global-emojify-mode)))


(provide 'js-ui)

;;; js-ui.el ends here
