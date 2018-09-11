;;; js-elixir.el --- Part of my emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package elixir-mode
  :commands elixir-mode
  :mode "\\.exs?")

(use-package alchemist
  :hook (elixir-mode . alchemist-mode)
  :general
  (space-leader-def 'normal elixir-mode-map
    ;; TODO FIXME all of these need to be prepended with m to scope to the major mode
    ;; "el" 'alchemist-eval-current-line
    ;; "eL" 'alchemist-eval-print-current-line
    ;; "er" 'alchemist-eval-region
    ;; "eR" 'alchemist-eval-print-region
    ;; "eb" 'alchemist-eval-buffer
    ;; "eB" 'alchemist-eval-print-buffer
    ;; "ej" 'alchemist-eval-quoted-current-line
    ;; "eJ" 'alchemist-eval-print-quoted-current-line
    ;; "eu" 'alchemist-eval-quoted-region
    ;; "eU" 'alchemist-eval-print-quoted-region
    ;; "ev" 'alchemist-eval-quoted-buffer
    ;; "eV" 'alchemist-eval-print-quoted-buffer

    ;; "gt" 'alchemist-project-toggle-file-and-tests
    ;; "gT" 'alchemist-project-toggle-file-and-tests-other-window

    ;; "h:" 'alchemist-help
    ;; "hH" 'alchemist-help-history
    ;; "hh" 'alchemist-help-search-at-point
    ;; "hr" 'alchemist-help--search-marked-region

    "m:" 'alchemist-mix
    "mc" 'alchemist-mix-compile
    "mx" 'alchemist-mix-run

    ;; "'"  'alchemist-iex-run
    ;; "sc" 'alchemist-iex-compile-this-buffer
    ;; "si" 'alchemist-iex-run
    ;; "sI" 'alchemist-iex-project-run
    ;; "sl" 'alchemist-iex-send-current-line
    ;; "sL" 'alchemist-iex-send-current-line-and-go
    ;; "sm" 'alchemist-iex-reload-module
    ;; "sr" 'alchemist-iex-send-region
    ;; "sR" 'alchemist-iex-send-region-and-go

    ;; "ta" 'alchemist-mix-test
    ;; "tb" 'alchemist-mix-test-this-buffer
    ;; "tB" 'alchemist-project-run-tests-for-current-file
    ;; "tt" 'alchemist-mix-test-at-point
    ;; "tF" 'alchemist-project-find-test
    ;; "tf" 'alchemist-mix-test-file
    ;; "tn" 'alchemist-test-mode-jump-to-next-test
    ;; "tN" 'alchemist-test-mode-jump-to-previous-test
    ;; "tr" 'alchemist-mix-rerun-last-test
    ;; "ts" 'alchemist-mix-test-stale
    ;; "tR" 'alchemist-test-toggle-test-report-display

    ;; "xb" 'alchemist-execute-this-buffer
    ;; "xf" 'alchemist-execute-file
    ;; "x:" 'alchemist-execute

    ;; "cb" 'alchemist-compile-this-buffer
    ;; "cf" 'alchemist-compile-file
    ;; "c:" 'alchemist-compile

    ;; "gg" 'alchemist-goto-definition-at-point
    ;; "." 'alchemist-goto-definition-at-point
    ;; "gb" 'alchemist-goto-jump-back
    ;; ","  'alchemist-goto-jump-back
    ;; "gN" 'alchemist-goto-jump-to-previous-def-symbol
    ;; "gn" 'alchemist-goto-jump-to-next-def-symbol
    ;; "gj" 'alchemist-goto-list-symbol-definitions

    ;; "Xi" 'alchemist-hex-info-at-point
    ;; "Xr" 'alchemist-hex-releases-at-point
    ;; "XR" 'alchemist-hex-releases
    ;; "XI" 'alchemist-hex-info
    ;; "Xs" 'alchemist-hex-search

    ;; "ol" 'alchemist-macroexpand-once-current-line
    ;; "oL" 'alchemist-macroexpand-once-print-current-line
    ;; "ok" 'alchemist-macroexpand-current-line
    ;; "oK" 'alchemist-macroexpand-print-current-line
    ;; "oi" 'alchemist-macroexpand-once-region
    ;; "oI" 'alchemist-macroexpand-once-print-region
    ;; "or" 'alchemist-macroexpand-region
    ;; "oR" 'alchemist-macroexpand-print-region
    ))

(use-package flycheck-mix
  :hook (elixir-mode . flycheck-mix-setup))


(provide 'js-elixir)
;;; js-elixir.el ends here
