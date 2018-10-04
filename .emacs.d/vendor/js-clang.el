;;; js-clang.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; C (via irony-mode)
(use-package irony
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode))
  :config
  (progn
    (setq irony-additional-clang-options '("-std=c++11"))
    (setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                                    iron-cdb-libclang))

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  (with-eval-after-load 'smartparens
    (sp-with-modes '(c++-mode objc-mode)
      (sp-local-pair "<" ">"
                     :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                     :post-handlers '(("| " "SPC"))))
    (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
      (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))))

(use-package irony-eldoc
  :hook (irony-mode . irony-eldoc))

(use-package flycheck-irony
  :hook (irony-mode . flycheck-irony-setup))
;; (use-package lsp-clangd
;;   :load-path "/vendor"
;;   :hook ((c-mode . lsp-clangd-c-enable)
;;          (c++-mode . lsp-clangd-c++-enable)
;;          (objc-mode . lsp-clangd-objc-enable)))
(use-package platformio-mode
  :after irony-mode
  :hook ((c-mode . platformio-conditionally-enable)
         (c++-mode . platformio-conditionally-enable)))

(use-package clang-format
  :after irony
  :config
  (progn
    (defun c-mode-before-save-hook ()
      (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
        (call-interactively 'clang-format)))

    (add-hook 'before-save-hook #'c-mode-before-save-hook)))

(use-package arduino-mode
  :after irony
  :config
  (add-to-list 'irony-supported-major-modes 'arduino-mode)
  (add-to-list 'irony-lang-compile-option-alist '(arduino-mode . "c++")))

(provide 'js-clang)

;;; js-clang.el ends here
