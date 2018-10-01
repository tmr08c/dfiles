;;; js-golang.el -- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package go-mode
  :mode "\\.go$"
  :requires (company)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun my-go-mode-hook-fn ()
    (go-eldoc-setup)
    ;; (set (make-local-variable 'company-backends) '(company-go))
    (setq-local 'company-backends '(company-go)
                tab-width 2
                indent-tabs-mode 1)
    (flycheck-gometalinter-setup)
    (flycheck-mode 1))
  (add-hook 'go-mode-hook #'my-go-mode-hook-fn)
  (keymap-for-mode 'go-mode
                   "t" '(:ignore t :which-key "test")
                   "ta" '(js/go-run-test-current-suite :which-key "run suite")
                   "tt" '(js/go-run-test-current-function :which-key "run current function")
                   "tg" '(:ignore t :which-key "generate")
                   "tgf" '(go-gen-test-exported :which-key "all exported functions")
                   "tga" '(go-gen-test-all :which-key "all functions")
                   "tgs" '(go-gen-test-dwim :which-key "selected region")

                   ;; Go To
                   "g" '(:ignore t :which-key "goto")
                   "gc" '(go-coverage :which-key "coverage")

                   ;; Imports
                   "i" '(:ignore t :which-key "imports")
                   "ia" '(go-import-add :which-key "add")
                   "ig" '(go-import-add :which-key "goto")
                   "ir" '(go-remove-unused-imports :which-key "remove unused")

                   ;; Execute
                   "x" '(:ignore t :which-key "execute")
                   "xx" '(js/go-run-main :which-key "run main")

                   ;; Refactoring
                   "r" '(:ignore t :which-key "refactoring")
                   "ri" '(go-impl :which-key "implement interface")
                   "rs" '(go-fill-struct :which-key "fill struct")
                   "rd" '(godoctor-godoc :which-key "godoc")
                   "re" '(godoctor-extract :which-key "extract")
                   "rn" '(godoctor-rename :which-key "rename")
                   ;; "rN" '(go-rename :which-key "rename")
                   "rt" '(godoctor-toggle :which-key "toggle")

                   ;; Help
                   "h" '(:ignore t :which-key "help")
                   "hh" '(godoc-at-point :which-key "godoc at point"))
  :custom
  (gofmt-command "goimports")
  ;; :ensure-system-package
  ;; ((gocode . "go get -u github.com/mdempsky/gocode")
  ;;  (gometalinter . "go get -u github.com/alecthomas/gometalinter")
  ;;  (godoc . "go get -u golang.org/x/tools/cmd/godoc")
  ;;  (goimports . "go get -u golang.org/x/tools/cmd/goimports")
  ;;  (guru . "go get -u golang.org/x/tools/cmd/guru"))
  )

(use-package go-eldoc
  :commands go-eldoc-setup)

(use-package flycheck-gometalinter
  :commands flycheck-gometalinter-setup
  ;; :hook (go-mode . flycheck-gometalinter-setup)
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
  :hook (go-mode . go-projectile-mode))

(use-package go-gen-test
  :commands (go-gen-test-exported
             go-gen-test-all
             go-gen-test-dwim)
  ;; :ensure-system-package
  ;; (gotests . "go get -u github.com/cweill/gotests/...")
  )

(use-package go-fill-struct
  :commands (go-fill-struct)
  ;; :ensure-system-package
  ;; (fillstruct . "go get -u github.com/davidrjenni/reftools/cmd/fillstruct")
  )

 (use-package godoctor
   :commands (godoctor-godoc
              godoctor-extract
              godoctor-rename
              godoctor-toggle))

(use-package go-rename
  :commands (go-rename)
  ;; :ensure-system-package
  ;; (gorename . "go get -u golang.org/x/tools/cmd/gorename")
  )

(use-package go-impl
  :commands go-impl
  ;; :ensure-system-package
  ;; (impl . "go get -u github.com/josharian/impl")
  )

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
