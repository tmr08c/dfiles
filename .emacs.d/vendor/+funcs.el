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

(cl-defun js|files-in
    (path-or-paths &rest rest
                   &key
                   filter
                   map
                   full
                   nosort
                   (follow-symlinks t)
                   (type 'files)
                   (relative-to (unless full default-directory))
                   (depth 99999)
                   (mindepth 0)
                   (match "/[^.]"))
  "Returns a list of files/directories in PATH-OR-PATHS (one string path or a
list of them).

FILTER is a function or symbol that takes one argument (the path). If it returns
non-nil, the entry will be excluded.

MAP is a function or symbol which will be used to transform each entry in the
results.

TYPE determines what kind of path will be included in the results. This can be t
(files and folders), 'files or 'dirs.

By default, this function returns paths relative to PATH-OR-PATHS if it is a
single path. If it a list of paths, this function returns absolute paths.
Otherwise, by setting RELATIVE-TO to a path, the results will be transformed to
be relative to it.

The search recurses up to DEPTH and no further. DEPTH is an integer.

MATCH is a string regexp. Only entries that match it will be included."
  (cond
   ((listp path-or-paths)
    (cl-loop for path in path-or-paths
             if (file-directory-p path)
             nconc (apply #'js|files-in path (plist-put rest :relative-to relative-to))))
   ((let ((path path-or-paths)
          result)
      (when (file-directory-p path)
        (dolist (file (directory-files path nil "." nosort))
          (unless (member file '("." ".."))
            (let ((fullpath (expand-file-name file path)))
              (cond ((file-directory-p fullpath)
                     (when (and (memq type '(t dirs))
                                (string-match-p match fullpath)
                                (not (and filter (funcall filter fullpath)))
                                (not (and (file-symlink-p fullpath)
                                          (not follow-symlinks)))
                                (<= mindepth 0))
                       (setq result
                             (nconc result
                                    (list (cond (map (funcall map fullpath))
                                                (relative-to (file-relative-name fullpath relative-to))
                                                (fullpath))))))
                     (unless (< depth 1)
                       (setq result
                             (nconc result (apply #'js|files-in fullpath
                                                  (append `(:mindepth ,(1- mindepth)
                                                                      :depth ,(1- depth)
                                                                      :relative-to ,relative-to)
                                                          rest))))))
                    ((and (memq type '(t files))
                          (string-match-p match fullpath)
                          (not (and filter (funcall filter fullpath)))
                          (<= mindepth 0))
                     (push (if relative-to
                               (file-relative-name fullpath relative-to)
                             fullpath)
                           result))))))
        result)))))

(provide '+funcs)
