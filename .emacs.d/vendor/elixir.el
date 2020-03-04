;;; elixir.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Erlang / Elixir
(use-package erlang)
(use-package elixir-mode
  :config
  (progn
    ;; (defun exs-freeze-workaround ()
    ;;   (when (and (stringp buffer-file-name)
    ;;              (string-match "\\.exs\\'" buffer-file-name))
    ;;     (auto-composition-mode -1)
    ;;     (elixir-mode)))
    ;; (if (version<= "27.0" emacs-version) (exs-freeze-workaround))
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'elixir-format nil t))
              )))
(use-package exunit
  :commands (exunit-verify-all
             exunit-verify-all-in-umbrella
             exunit-verify
             exunit-verify-single
             exunit-rerun))
(use-package alchemist
  :disabled
  :hook (elixir-mode . alchemist-mode)
  :config
  (setq alchemist-project-compile-when-needed t
        alchemist-test-status-modeline t
        alchemist-test-truncate-lines nil)
  (dolist (mode (list alchemist-compile-mode-map
                      alchemist-eval-mode-map
                      alchemist-execute-mode-map
                      alchemist-message-mode-map
                      alchemist-help-minor-mode-map
                      alchemist-mix-mode-map
                      alchemist-macroexpand-mode-map
                      alchemist-refcard-mode-map
                      alchemist-test-report-mode-map))
    (evil-define-key 'normal mode
      (kbd "q") 'quit-window)))
(use-package alchemist-company
  :disabled
  :ensure nil
  :hook (elixir-mode . (lambda ()
                         (setq-local company-backends '(alchemist-company company-yasnippet)))))
(use-package flycheck-credo
  :hook (elixir-mode . flycheck-credo-setup))
(use-package flycheck-mix
  :hook (elixir-mode . flycheck-mix-setup))

(js|keymap-for-mode 'elixir-mode
                    "t" '(:ignore t :wk "test")
                    "tb" 'exunit-verify-all
                    "tB" 'exunit-verify-all-in-umbrella
                    "ta" 'exunit-verify
                    "tk" 'exunit-rerun
                    "tt" 'exunit-verify-single)
;; NOTE outdated from alchemist set up. Here for reference

;; (js|keymap-for-mode 'elixir-mode
;;                     ;; "f" 'elixir-format

;;                     "e" '(:ignore t :wk "eval")
;;                     "el" 'alchemist-eval-current-line
;;                     "eL" 'alchemist-eval-print-current-line
;;                     "er" 'alchemist-eval-region
;;                     "eR" 'alchemist-eval-print-region
;;                     "eb" 'alchemist-eval-buffer
;;                     "eB" 'alchemist-eval-print-buffer
;;                     "ej" 'alchemist-eval-quoted-current-line
;;                     "eJ" 'alchemist-eval-print-quoted-current-line
;;                     "eu" 'alchemist-eval-quoted-region
;;                     "eU" 'alchemist-eval-print-quoted-region
;;                     "ev" 'alchemist-eval-quoted-buffer
;;                     "eV" 'alchemist-eval-print-quoted-buffer

;;                     "g" '(:ignore t :wk "goto")
;;                     "gt" 'alchemist-project-toggle-file-and-tests
;;                     "gT" 'alchemist-project-toggle-file-and-tests-other-window
;;                     "gg" 'alchemist-goto-definition-at-point
;;                     ;; "." 'alchemist-goto-definition-at-point
;;                     "gb" 'alchemist-goto-jump-back
;;                     ;; ","  'alchemist-goto-jump-back
;;                     "gN" 'alchemist-goto-jump-to-previous-def-symbol
;;                     "gn" 'alchemist-goto-jump-to-next-def-symbol
;;                     "gj" 'alchemist-goto-list-symbol-definitions


;;                     "h" '(:ignore t :wk "help")
;;                     "h:" 'alchemist-help
;;                     "hH" 'alchemist-help-history
;;                     "hh" 'alchemist-help-search-at-point
;;                     "hr" 'alchemist-help--search-marked-region

;;                     "m" '(:ignore t :wk "mix")
;;                     "m:" 'alchemist-mix
;;                     "mc" 'alchemist-mix-compile
;;                     "mx" 'alchemist-mix-run

;;                     "s" '(:ignore t :wk "iex")
;;                     ;; "'"  'alchemist-iex-run
;;                     "sc" 'alchemist-iex-compile-this-buffer
;;                     "si" 'alchemist-iex-run
;;                     "sI" 'alchemist-iex-project-run
;;                     "sl" 'alchemist-iex-send-current-line
;;                     "sL" 'alchemist-iex-send-current-line-and-go
;;                     "sm" 'alchemist-iex-reload-module
;;                     "sr" 'alchemist-iex-send-region
;;                     "sR" 'alchemist-iex-send-region-and-go

;;                     "t" '(:ignore t :wk "test")
;;                     "ta" 'alchemist-mix-test
;;                     "tb" 'alchemist-mix-test-this-buffer
;;                     "tB" 'alchemist-project-run-tests-for-current-file
;;                     "ts" 'alchemist-mix-test-at-point
;;                     "tF" 'alchemist-project-find-test
;;                     "tf" 'alchemist-mix-test-file
;;                     "tn" 'alchemist-test-mode-jump-to-next-test
;;                     "tN" 'alchemist-test-mode-jump-to-previous-test
;;                     "tr" 'alchemist-mix-rerun-last-test
;;                     "tS" 'alchemist-mix-test-stale
;;                     "tR" 'alchemist-test-toggle-test-report-display

;;                     "x" '(:ignore t :wk "execute")
;;                     "xb" 'alchemist-execute-this-buffer
;;                     "xf" 'alchemist-execute-file
;;                     "x:" 'alchemist-execute

;;                     "c" '(:ignore t :wk "compile")
;;                     "cb" 'alchemist-compile-this-buffer
;;                     "cf" 'alchemist-compile-file
;;                     "c:" 'alchemist-compile

;;                     "X" '(:ignore t :wk "hex")
;;                     "Xi" 'alchemist-hex-info-at-point
;;                     "Xr" 'alchemist-hex-releases-at-point
;;                     "XR" 'alchemist-hex-releases
;;                     "XI" 'alchemist-hex-info
;;                     "Xs" 'alchemist-hex-search

;;                     "o" '(:ignore t :wk "macroexpand")
;;                     "ol" 'alchemist-macroexpand-once-current-line
;;                     "oL" 'alchemist-macroexpand-once-print-current-line
;;                     "ok" 'alchemist-macroexpand-current-line
;;                     "oK" 'alchemist-macroexpand-print-current-line
;;                     "oi" 'alchemist-macroexpand-once-region
;;                     "oI" 'alchemist-macroexpand-once-print-region
;;                     "or" 'alchemist-macroexpand-region
;;                     "oR" 'alchemist-macroexpand-print-region)
(provide 'elixir)
;;; elixir.el ends here
