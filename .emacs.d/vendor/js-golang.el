;;; js-golang.el -- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package go-mode
  :mode "\\.go$"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  :custom
  (tab-width 2)
  (indent-tabs-mode 1)
  (gofmt-command "goimports")
  :ensure-system-package
  ((gocode . "go get -u github.com/mdempsky/gocode")
   (gometalinter . "go get -u github.com/alecthomas/gometalinter")
   (godoc . "go get -u golang.org/x/tools/cmd/godoc")
   (goimports . "go get -u golang.org/x/tools/cmd/goimports")
   (guru . "go get -u golang.org/x/tools/cmd/guru"))
  :general
  (space-leader-def 'normal go-mode-map
    ;; Tests
    "m t" '(:ignore t :which-key "test")
    "m t a" '(js/go-run-test-current-suite :which-key "run suite")
    "m t t" '(js/go-run-test-current-function :which-key "run current function")
    "m t g" '(:ignore t :which-key "generate")
    "m t g f" '(go-gen-test-exported :which-key "all exported functions")
    "m t g a" '(go-gen-test-all :which-key "all functions")
    "m t g s" '(go-gen-test-dwim :which-key "selected region")

    ;; Go To
    "m g" '(:ignore t :which-key "goto")
    "m g c" '(go-coverage :which-key "coverage")

    ;; Imports
    "m i" '(:ignore t :which-key "imports")
    "m i a" '(go-import-add :which-key "add")
    "m i g" '(go-import-add :which-key "goto")
    "m i r" '(go-remove-unused-imports :which-key "remove unused")

    ;; Execute
    "m x" '(:ignore t :which-key "execute")
    "m x x" '(js/go-run-main :which-key "run main")

    ;; Refactoring
    "m r" '(:ignore t :which-key "refactoring")
    "m r i" '(go-impl :which-key "implement interface")
    "m r s" '(go-fill-struct :which-key "fill struct")
    "m r d" '(godoctor-godoc :which-key "godoc")
    "m r e" '(godoctor-extract :which-key "extract")
    "m r n" '(godoctor-rename :which-key "rename")
    ;; "m r N" '(go-rename :which-key "rename")
    "m r t" '(godoctor-toggle :which-key "toggle")

    ;; Help
    "m h" '(:ignore t :which-key "help")
    "m h h" '(godoc-at-point :which-key "godoc at point")
    ))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(use-package flycheck-gometalinter
  :hook (go-mode . flycheck-gometalinter-setup)
  :custom
  ;; skip linting for vendor dirs
  (flycheck-gometalinter-vendor t)
  ;; use in test files
  (flycheck-gometalinter-test t)
  ;; only use fast linters
  (flycheck-gometalinter-fast t)
  ;; explicitly disable 'gotype' & 'govet' linters (also currently broken Nix overlays)
  (flycheck-gometalinter-disable-linters
   '("gosec" "gotype" "vet" "vetshadow" "megacheck" "interfacer" "ineffassign")))

(use-package go-projectile
  :load-path "vendor/"
  :hook (go-mode . go-projectile-mode))

(use-package go-gen-test
  :commands (go-gen-test-exported
             go-gen-test-all
             go-gen-test-dwim)
  :ensure-system-package
  (gotests . "go get -u github.com/cweill/gotests/..."))

(use-package go-fill-struct
  :commands (go-fill-struct)
  :ensure-system-package
  (fillstruct . "go get -u github.com/davidrjenni/reftools/cmd/fillstruct"))

 (use-package godoctor
   :commands (godoctor-godoc
              godoctor-extract
              godoctor-rename
              godoctor-toggle))

(use-package go-rename
  :commands (go-rename)
  :ensure-system-package
  (gorename . "go get -u golang.org/x/tools/cmd/gorename"))

(use-package go-impl
  :commands go-impl
  :ensure-system-package
  (impl . "go get -u github.com/josharian/impl"))

;; Taken from js
(defun js/go-run-tests (args)
  (interactive)
  (compilation-start (concat "go test " args " " go-use-test-args)
                     nil (lambda (n) go-test-buffer-name) nil))

(defun js/go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (let ((test-method (if go-use-gocheck-for-testing
                             "-check.f"
                           "-run")))
        (save-excursion
          (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
          (js/go-run-tests (concat test-method "='" (match-string-no-properties 2) "$'"))))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

(defun js/go-run-test-current-suite ()
  (interactive)
  (if (string-match "_test\.go" buffer-file-name)
      (if go-use-gocheck-for-testing
          (save-excursion
            (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
            (js/go-run-tests (concat "-check.f='" (match-string-no-properties 2) "'")))
        (message "Gocheck is needed to test the current suite"))
    (message "Must be in a _test.go file to run go-test-current-suite")))


(defun js/go-run-main ()
  (interactive)
  (shell-command
   (format "go run %s"
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer)))))))


(provide 'js-golang)
;;; js-golang.el ends here
