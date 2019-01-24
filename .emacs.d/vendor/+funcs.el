;;; -*- lexical-binding: t; -*-

(defmacro js|global-keymap (&rest bindings)
  "Add global BINDINGS as key bindings under `space-leader-def`.
All of the arguments are treated exactly like they are in
'general' package."
  `(space-leader-def
     :states '(normal visual emacs)
     ,@bindings))

(defmacro js|keymap-for-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under `space-leader-def` for MODE.
mode should be a quoted symbol corresponding to a valid major mode.
the rest of the arguments are treated exactly like they are in
'general' package."
  (let (mode-bindings)
    (while key
      (push def mode-bindings)
      (push (concat "m" key) mode-bindings)
      (setq key (pop bindings) def (pop bindings)))
    `(space-leader-def
       :states '(normal visual emacs)
       :keymaps ',(intern (format "%s-map" (eval mode)))
       ,@mode-bindings)))

(defmacro evil-js|keymap-for-mode (mode &rest bindings)
  "Add BINDINGS to evil for the provided MODE.
mode should be a quoted symbol corresponding to a valid major mode.
the rest of the arguments are treated exactly like they are in
'general' package."
  `(general-define-key
    :states '(normal visual)
    :keymaps ',(intern (format "%s-map" (eval mode)))
    ,@bindings))

;;;###autoload
(defun js|go-run-tests (args)
  (interactive)
  (compilation-start (concat "go test " args " " go-use-test-args)
                     nil (lambda (n) go-test-buffer-name) nil))

;;;###autoload
(defun js|go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)

      (let ((test-method (if go-use-gocheck-for-testing
                             "-check.f"
                           "-run")))
        (save-excursion
          (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
          (js|go-run-tests (concat test-method "='" (match-string-no-properties 2) "$'"))))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

;;;###autoload
(defun js|go-run-test-current-suite ()
  (interactive)
  (if (string-match "_test\.go" buffer-file-name)
      (if go-use-gocheck-for-testing
          (save-excursion
            (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
            (js|go-run-tests (concat "-check.f='" (match-string-no-properties 2) "'")))
        (message "Gocheck is needed to test the current suite"))
    (message "Must be in a _test.go file to run go-test-current-suite")))


;;;###autoload
(defun js|go-run-main ()
  (interactive)
  (shell-command
   (format "go run %s"
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer)))))))

(provide '+funcs)
