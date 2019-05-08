;;; -*- lexical-binding: t; -*-

(general-define-key :keymaps 'global
                    [remap describe-function] #'helpful-callable
                    [remap describe-command]  #'helpful-command
                    [remap describe-variable] #'helpful-variable
                    [remap describe-key] #'helpful-key)

(when (eq +completion-engine 'helm)
  (general-define-key :keymaps 'global
                      [remap projectile-find-file]        'helm-projectile-find-file
                      [remap projectile-find-dir]         'helm-projectile-find-dir
                      [remap projectile-switch-to-buffer] 'helm-projectile-switch-to-buffer
                      [remap projectile-grep]             'helm-projectile-grep
                      [remap projectile-ag]               'helm-projectile-rg
                      [remap projectile-switch-project]   'helm-projectile-switch-project
                      [remap apropos]                   'helm-apropos
                      [remap find-library]              'helm-locate-library
                      [remap bookmark-jump]             'helm-bookmarks
                      [remap execute-extended-command]  'helm-M-x
                      [remap find-file]                 'helm-find-files
                      [remap imenu-anywhere]            'helm-imenu-anywhere
                      [remap imenu]                     'helm-semantic-or-imenu
                      [remap noop-show-kill-ring]       'helm-show-kill-ring
                      ;; [remap persp-switch-to-buffer]    '+helm/workspace-mini
                      [remap switch-to-buffer]          'helm-buffers-list
                      [remap switch-buffer]          'helm-buffers-list
                      [remap projectile-recentf]        'helm-projectile-recentf
                      [remap projectile-switch-project] 'helm-projectile-switch-project
                      [remap projectile-switch-to-buffer] 'helm-projectile-switch-to-buffer
                      [remap recentf-open-files]        'helm-recentf
                      [remap yank-pop]                  'helm-show-kill-ring
                      [remap swiper]                    'swiper-helm)
  (with-eval-after-load 'helm-files
    (general-nmap 'helm-map
      "<tab>" 'helm-execute-persistent-action
      "TAB" 'helm-execute-persistent-action
      "C-z" 'helm-select-action)
    (general-nmap 'helm-find-files-map
      "S-TAB" 'helm-find-files-up-one-level
      "<backtab>" 'helm-find-files-up-one-level
      "S-<tab>" 'helm-find-files-up-one-level))
  (js|global-keymap
   "feD" '((lambda ()
             (interactive)
             (helm-find-files user-emacs-directory))
           :which-key "search emacs config directory")
   "Ts" 'helm-themes))

(when (eq +completion-engine 'ivy)
  (general-define-key :keymaps 'global
                      [remap apropos]                  'counsel-apropos
                      [remap bookmark-jump]            'counsel-bookmark
                      [remap describe-face]            'counsel-faces
                      [remap describe-function]        'counsel-describe-function
                      [remap describe-variable]        'counsel-describe-variable
                      [remap execute-extended-command] 'counsel-M-x
                      [remap find-file]                'counsel-find-file
                      [remap find-library]             'counsel-find-library
                      [remap info-lookup-symbol]       'counsel-info-lookup-symbol
                      [remap imenu]                    'counsel-imenu
                      [remap recentf-open-files]       'counsel-recentf
                      [remap org-capture]              'counsel-org-capture
                      [remap swiper]                   'counsel-grep-or-swiper
                      [remap projectile-find-file]        'counsel-projectile-find-file
                      [remap projectile-find-dir]         'counsel-projectile-find-dir
                      [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer
                      [remap projectile-grep]             'counsel-projectile-grep
                      [remap projectile-ag]               'counsel-projectile-rg
                      [remap projectile-switch-project]   'counsel-projectile-switch-project
                      [remap switch-to-buffer]       'ivy-switch-buffer
                      [remap switch-buffer]       'ivy-switch-buffer
                      ;; [remap persp-switch-to-buffer] '+ivy/switch-workspace-buffer
                      [remap imenu-anywhere]         'ivy-imenu-anywhere)
  (js|global-keymap
   "?"   '(counsel-descbinds :which-key "Help")
   "dd" '((lambda ()
            (interactive)
            (counsel-dash
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (substring-no-properties (or (thing-at-point 'symbol) "")))))
          :which-key "Lookup thing at point")
   "dD" '(counsel-dash :which-key "Lookup thing at point with docset")
   "feD" '((lambda ()
             (interactive)
             (counsel-find-file user-emacs-directory))
           :which-key "search emacs config directory")
   "Ts" '(counsel-load-theme :which-key "switch theme")
   "pT" '(doom/ivy-tasks :which-key "List project tasks")))


(general-define-key :states 'insert
                    "C-v" 'cua-paste
                    "C-c" 'cua-copy-region
                    "C-x" 'cua-cut-region
                    "C-z" 'undo-tree-undo
                    "C-Z" 'undo-tree-redo)

(js|global-keymap "'" '(eshell-toggle :which-key "toggle shell")
                  "`" 'winum-select-window-by-number
                  ;; "²" 'winum-select-window-by-number
                  "0" 'winum-select-window-0-or-10
                  "1" 'winum-select-window-1
                  "2" 'winum-select-window-2
                  "3" 'winum-select-window-3
                  "4" 'winum-select-window-4
                  "5" 'winum-select-window-5
                  "6" 'winum-select-window-6
                  "7" 'winum-select-window-7
                  "8" 'winum-select-window-8
                  "9" 'winum-select-window-9

                  "SPC" '(execute-extended-command :which-key "M-x")
                  ;; "TAB" '(switch-to-other-buffer :which-key "prev buffer")

                  ;; Help bindings
                  "h" '(:ignore t :which-key "Help")
                  "hdf" '(describe-function :which-key "describe function")
                  "hdm" '(describe-mode :which-key "describe modes") ;; TODO: https://framagit.org/steckerhalter/discover-my-major
                  "hds" '(yas-describe-tables :which-key "describve snippets")
                  "hdv" '(describe-variable :which-key "describe variable")

                  ;; Buffers
                  "b"   '(:ignore t :which-key "Buffers")
                  "bb" '(switch-buffer :which-key "list buffers")
                  "bn" '(next-buffer :which-key "next buffer")
                  "bp" '(previous-buffer :which-key "prev buffer")
                  "bd" '((lambda ()
                           (interactive)
                           (kill-buffer (current-buffer)))
                         :which-key "close current buffer")
                  "bs" '((lambda ()
                           (interactive)
                           (switch-to-buffer (get-buffer-create "*scratch*")))
                         :which-key "scratch buffer")

                  ;; Files
                  "f"  '(:ignore t :which-key "Files")
                  "fD" '((lambda ()
                           (interactive)
                           (let ((filename (buffer-file-name))
                                 (buffer (current-buffer))
                                 (name (buffer-name)))
                             (if (not (and filename (file-exists-p filename)))
                                 (ido-kill-buffer)
                               (when (yes-or-no-p "Are you sure you want to delete this file? ")
                                 (delete-file filename t)
                                 (kill-buffer buffer)
                                 (message "File '%s' successfully removed" filename)))))
                         :which-key "delete file and kill buffer")
                  "ff" '(find-file :which-key "find file")
                  "fed" '((lambda ()
                            (interactive)
                            (find-file-existing js|config-file))
                          :which-key "open emacs configuration")

                  "ft" 'neotree-toggle

                  ;; Docs
                  "d" '(:ignore t :which-key "Docs")

                  ;; Go to
                  "g" '(:ignore t :which-key "Go to")
                  "gd" '(dumb-jump-go :which-key "definition")
                  "gD" '(dumb-jump-go-other-window :which-key "definition (other window)")

                  "hh" '(:ignore t :which-key "helpful")
                  "hhh" 'helpful-at-point
                  "hhc" 'helpful-command
                  "hhf" 'helpful-callable
                  "hhk" 'helpful-key
                  "hhm" 'helpful-macro
                  "hhv" 'helpful-variable

                  ;; Layouts
                  "l" '(eyebrowse-hydra/body :which-key "Layouts")

                  ;; Project
                  "p"  '(:ignore t :which-key "Projects")
                  "p!" '(projectile-run-shell-command-in-root :which-key "run command")
                  "p%" '(projectile-replace-regexp :which-key "replace regexp")
                  ;; "p a" '(projectile-toggle-between-implementation-and-test :which-key "toggle test")
                  "pc" 'org-projectile-projectile-project-todo-completing-read
                  "pI" '(projectile-invalidate-cache :which-key "clear cache")
                  "pR" '(projectile-replace :which-key "replace")
                  "pk" '(projectile-kill-buffers :which-key "kill buffers")
                  "pr" '(projectile-recentf :which-key "recent files")
                  "pb" '(projectile-switch-to-buffer :which-key "switch to buffer")
                  "pd" '(projectile-find-dir :which-key "find directory")
                  "pf" '(projectile-find-file :which-key "open file")
                  "pp" '(projectile-switch-project :which-key "open project")
                  "ps" '(projectile-ag :which-key "search in project")
                  "pt" 'neotree-projectile-action

                  ;; Quit
                  "q"  '(:ignore t :which-key "Quit")
                  "qq" '(kill-emacs :which-key "quit")
                  "qr" '(restart-emacs :which-key "restart")

                  ;; Search
                  "s" '(:ignore t :which-key "Search")
                  "ss" '(swiper :which-key "search buffer")
                  "sS" '(lambda ()
                          (interactive)
                          (let ((input (if (region-active-p)
                                           (buffer-substring-no-properties
                                            (region-beginning) (region-end))
                                         (thing-at-point 'symbol t))))
                            (swiper input))
                          :which-key "search buffer")

                  "S" '(:ignore t :which-key "Spelling")
                  "Sb" 'flyspell-buffer
                  "Sn" 'flyspell-goto-next-error

                  ;; Toggle
                  "t" '(:ignore t :which-key "Toggles")
                  "tc" 'hide/show-comments-toggle
                  "th" '(:ignore t :which-key "highlight")
                  "thi" 'highlight-indentation-mode
                  "thc" 'highlight-indentation-current-column-mode
                  "tS" 'js|flyspell-mode-toggle
                  "ti" 'indent-guide-mode
                  "t TAB" 'indent-guide-global-mode

                  "u" 'universal-argument-more

                  ;; Windows
                  "w"   '(:ignore t :which-key "Windows")
                  "wd" '(delete-window :which-key "close window")
                  "w/" '((lambda ()
                           (interactive)
                           (split-window-horizontally)
                           (other-window 1))
                         :which-key "split vertical")
                  "w-" '((lambda ()
                           (interactive)
                           (split-window-vertically)
                           (other-window 1))
                         :which-key "split horizontal")
                  "wh" '(evil-window-left :which-key "window left")
                  "w<left>" '(evil-window-left :which-key nil)
                  "wj" '(evil-window-down :which-key "window down")
                  "w<down>" '(evil-window-down :which-key nil)
                  "wk" '(evil-window-up :which-key "window up")
                  "w<up>" '(evil-window-up :which-key nil)
                  "wl" '(evil-window-right :which-key "window right")
                  "w<right>" '(evil-window-right :which-key nil)
                  "w=" '(balance-windows :which-key "balance window split")

                  "x" '(:ignore t :which-key "text")
                  "xg" '(:ignore t :which-key "google translate")
                  "xgl" 'spacemacs/set-google-translate-languages
                  "xgQ" 'google-translate-query-translate-reverse
                  "xgq" 'google-translate-query-translate
                  "xgT" 'google-translate-at-point-reverse
                  "xgt" 'google-translate-at-point

                  "xt" '(:ignore t :which-key "transpose"))
;; (js|keymap-for-mode 'elixir-mode
;;                     "t" '(:ignore t :which-key "test")
;;                     "tb" 'exunit-verify-all
;;                     "tB" 'exunit-verify-all-in-umbrella
;;                     "ta" 'exunit-verify
;;                     "tk" 'exunit-rerun
;;                     "tt" 'exunit-verify-single)
;; NOTE outdated from alchemist set up. Here for reference
(js|keymap-for-mode 'elixir-mode
                    ;; "f" 'elixir-format

                    "e" '(:ignore t :which-key "eval")
                    "el" 'alchemist-eval-current-line
                    "eL" 'alchemist-eval-print-current-line
                    "er" 'alchemist-eval-region
                    "eR" 'alchemist-eval-print-region
                    "eb" 'alchemist-eval-buffer
                    "eB" 'alchemist-eval-print-buffer
                    "ej" 'alchemist-eval-quoted-current-line
                    "eJ" 'alchemist-eval-print-quoted-current-line
                    "eu" 'alchemist-eval-quoted-region
                    "eU" 'alchemist-eval-print-quoted-region
                    "ev" 'alchemist-eval-quoted-buffer
                    "eV" 'alchemist-eval-print-quoted-buffer

                    "g" '(:ignore t :which-key "goto")
                    "gt" 'alchemist-project-toggle-file-and-tests
                    "gT" 'alchemist-project-toggle-file-and-tests-other-window
                    "gg" 'alchemist-goto-definition-at-point
                    ;; "." 'alchemist-goto-definition-at-point
                    "gb" 'alchemist-goto-jump-back
                    ;; ","  'alchemist-goto-jump-back
                    "gN" 'alchemist-goto-jump-to-previous-def-symbol
                    "gn" 'alchemist-goto-jump-to-next-def-symbol
                    "gj" 'alchemist-goto-list-symbol-definitions


                    "h" '(:ignore t :which-key "help")
                    "h:" 'alchemist-help
                    "hH" 'alchemist-help-history
                    "hh" 'alchemist-help-search-at-point
                    "hr" 'alchemist-help--search-marked-region

                    "m" '(:ignore t :which-key "mix")
                    "m:" 'alchemist-mix
                    "mc" 'alchemist-mix-compile
                    "mx" 'alchemist-mix-run

                    "s" '(:ignore t :which-key "iex")
                    ;; "'"  'alchemist-iex-run
                    "sc" 'alchemist-iex-compile-this-buffer
                    "si" 'alchemist-iex-run
                    "sI" 'alchemist-iex-project-run
                    "sl" 'alchemist-iex-send-current-line
                    "sL" 'alchemist-iex-send-current-line-and-go
                    "sm" 'alchemist-iex-reload-module
                    "sr" 'alchemist-iex-send-region
                    "sR" 'alchemist-iex-send-region-and-go

                    "t" '(:ignore t :which-key "test")
                    "ta" 'alchemist-mix-test
                    "tb" 'alchemist-mix-test-this-buffer
                    "tB" 'alchemist-project-run-tests-for-current-file
                    "tt" 'alchemist-mix-test-at-point
                    "tF" 'alchemist-project-find-test
                    "tf" 'alchemist-mix-test-file
                    "tn" 'alchemist-test-mode-jump-to-next-test
                    "tN" 'alchemist-test-mode-jump-to-previous-test
                    "tr" 'alchemist-mix-rerun-last-test
                    "ts" 'alchemist-mix-test-stale
                    "tR" 'alchemist-test-toggle-test-report-display

                    "x" '(:ignore t :which-key "execute")
                    "xb" 'alchemist-execute-this-buffer
                    "xf" 'alchemist-execute-file
                    "x:" 'alchemist-execute

                    "c" '(:ignore t :which-key "compile")
                    "cb" 'alchemist-compile-this-buffer
                    "cf" 'alchemist-compile-file
                    "c:" 'alchemist-compile

                    "X" '(:ignore t :which-key "hex")
                    "Xi" 'alchemist-hex-info-at-point
                    "Xr" 'alchemist-hex-releases-at-point
                    "XR" 'alchemist-hex-releases
                    "XI" 'alchemist-hex-info
                    "Xs" 'alchemist-hex-search

                    "o" '(:ignore t :which-key "macroexpand")
                    "ol" 'alchemist-macroexpand-once-current-line
                    "oL" 'alchemist-macroexpand-once-print-current-line
                    "ok" 'alchemist-macroexpand-current-line
                    "oK" 'alchemist-macroexpand-print-current-line
                    "oi" 'alchemist-macroexpand-once-region
                    "oI" 'alchemist-macroexpand-once-print-region
                    "or" 'alchemist-macroexpand-region
                    "oR" 'alchemist-macroexpand-print-region)

(js|keymap-for-mode 'pdf-view
                    ;; Slicing image
                    "sm" 'pdf-view-set-slice-using-mouse
                    "sb" 'pdf-view-set-slice-from-bounding-box
                    "sr" 'pdf-view-reset-slice
                    ;; Annotations
                    "a" '(:ignore t :which-key "annotations")
                    "aD" 'pdf-annot-delete
                    "at"	'pdf-annot-attachment-dired
                    "ah"	'pdf-annot-add-highlight-markup-annotation
                    "al"	'pdf-annot-list-annotations
                    "am"	'pdf-annot-add-markup-annotation
                    "ao"	'pdf-annot-add-strikeout-markup-annotation
                    "as"	'pdf-annot-add-squiggly-markup-annotation
                    "at"	'pdf-annot-add-text-annotation
                    "au"	'pdf-annot-add-underline-markup-annotation
                    ;; Fit image to window
                    "f" '(:ignore t :which-key "fit")
                    "fw" 'pdf-view-fit-width-to-window
                    "fh" 'pdf-view-fit-height-to-window
                    "fp" 'pdf-view-fit-page-to-window
                    ;; Other
                    "s" '(:ignore t :which-key "slice/search")
                    "ss" 'pdf-occur
                    "p" 'pdf-misc-print-document
                    "O" 'pdf-outline
                    "n" 'pdf-view-midnight-minor-mode)

(js|keymap-for-mode 'go-mode
                    "t" '(:ignore t :which-key "test")
                    "ta" '(js|go-run-test-current-suite :which-key "run suite")
                    "tt" '(js|go-run-test-current-function :which-key "run current function")

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
                    "xx" '(js|go-run-main :which-key "run main")

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

(js|keymap-for-mode 'json-mode
                    "=" 'spacemacs/json-reformat-dwim)

(js|keymap-for-mode 'adoc-mode
                    "h" '(:ignore t :which-key "headers")
                    "h1" 'tempo-template-adoc-title-1
                    ;; Alternative method of inserting top-level heading
                    "hI" 'tempo-template-adoc-title-1
                    "h2" 'tempo-template-adoc-title-2
                    ;; Alternative method of inserting the most usual heading
                    "hi" 'tempo-template-adoc-title-2
                    "h3" 'tempo-template-adoc-title-3
                    "h4" 'tempo-template-adoc-title-4
                    "h5" 'tempo-template-adoc-title-5

                    "x" '(:ignore t :which-key "text")
                    "xb" 'tempo-template-adoc-strong
                    "xi" 'tempo-template-adoc-emphasis

                    "p" 'adoc-promote
                    "d" 'adoc-demote)

(js|keymap-for-mode 'elm-mode
                    "'"  'elm-repl-load
                    ;; format
                    "=b" 'elm-mode-format-buffer

                    "c" '(:ignore t :which-key "compile")
                    "cb" 'elm-compile-buffer
                    ;; "cB" 'spacemacs/elm-compile-buffer-output
                    "cm" 'elm-compile-main

                    "h" '(:ignore t :which-key "help")
                    "hh" 'elm-oracle-doc-at-point
                    "ht" 'elm-oracle-type-at-point
                    ;; refactoring
                    "r" '(:ignore t :which-key "refactor")
                    "ri" 'elm-sort-imports

                    "s" '(:ignore t :which-key "repl")
                    "si" 'elm-repl-load
                    "sf" 'elm-repl-push-decl
                    ;; "sF" 'spacemacs/elm-repl-push-decl-focus
                    "sr" 'elm-repl-push
                    ;; "sR" 'spacemacs/elm-repl-push-focus

                    "R" '(:ignore t :which-key "reactor")
                    "Rn" 'elm-preview-buffer
                    "Rm" 'elm-preview-main

                    "p" '(:ignore t :which-key "package")
                    "pi" 'elm-import
                    "pc" 'elm-package-catalog
                    "pd" 'elm-documentation-lookup

                    "t" '(:ignore t :which-key "test")
                    "tb" 'elm-test-runner-run
                    "td" 'elm-test-runner-run-directory
                    "tp" 'elm-test-runner-run-project
                    "tr" 'elm-test-runner-rerun
                    "tw" 'elm-test-runner-watch
                    "t TAB" 'elm-test-runner-toggle-test-and-target)

(js|keymap-for-mode 'reason-mode
                    ;; Not complete
                    ;; See spacemacs PR to follow status
                    ;; https://github.com/syl20bnr/spacemacs/pull/11963
                    "c" '(:ignore t :which-key "compile")
                    "cp" 'merlin-project-check
                    "cv" 'merlin-goto-project-file
                    "e" '(:ignore t :which-key "errors/eval")
                    "eC" 'merlin-error-check
                    "en" 'merlin-error-next
                    "eN" 'merlin-error-prev

                    "g" '(:ignore t :which-key "goto")
                    "gb" 'merlin-pop-stack
                    "gg" 'merlin-locate
                    ;; "gG" 'spacemacs/merlin-locate-other-window
                    "gl" 'merlin-locate-ident
                    "gi" 'merlin-switch-to-ml
                    "gI" 'merlin-switch-to-mli
                    "go" 'merlin-occurrences

                    "h" '(:ignore t :which-key "help")
                    "hh" 'merlin-document
                    "ht" 'merlin-type-enclosing
                    "hT" 'merlin-type-expr

                    "r" '(:ignore t :which-key "refactor")
                    "rd" 'merlin-destruct)

(js|keymap-for-mode 'ruby-mode
                    "b" '(:ignore t :which-key "bundle")
                    "bc" 'bundle-check
                    "bi" 'bundle-install
                    "bs" 'bundle-console
                    "bu" 'bundle-update
                    "bx" 'bundle-exec
                    "bo" 'bundle-open

                    "fh" 'ruby-hash-syntax-toggle

                    "rr" '(:ignore t :which-key "Rubocop")
                    "rrd" 'rubocop-check-directory
                    "rrD" 'rubocop-autocorrect-directory
                    "rrf" 'rubocop-check-current-file
                    "rrF" 'rubocop-autocorrect-current-file
                    "rrp" 'rubocop-check-project
                    "rrP" 'rubocop-autocorrect-project

                    "s" '(:ignore t :which-key "repl")
                    "sb" 'ruby-send-buffer
                    "sB" 'ruby-send-buffer-and-go
                    "sf" 'ruby-send-definition
                    "sF" 'ruby-send-definition-and-go
                    "sl" 'ruby-send-line
                    "sL" 'ruby-send-line-and-go
                    "sr" 'ruby-send-region
                    "sR" 'ruby-send-region-and-go
                    "ss" 'ruby-switch-to-inf

                    "t" '(:ignore t :which-key "test")
                    "ta" 'rspec-verify-all
                    "tb" 'rspec-verify
                    "tc" 'rspec-verify-continue
                    "td" 'ruby/rspec-verify-directory
                    "te" 'rspec-toggle-example-pendingness
                    "tf" 'rspec-verify-method
                    "tl" 'rspec-run-last-failed
                    "tm" 'rspec-verify-matching
                    "tr" 'rspec-rerun
                    "tt" 'rspec-verify-single
                    "t~" 'rspec-toggle-spec-and-target-find-example
                    "t TAB" 'rspec-toggle-spec-and-target

                    "T" '(:ignore t :which-key "toggle")
                    "T'" 'ruby-toggle-string-quotes
                    "T{" 'ruby-toggle-block)

(js|keymap-for-mode 'lisp-mode
                    "'" 'sly

                    "h" '(:ignore t :which-key "help")
                    "ha" 'sly-apropos
                    "hb" 'sly-who-binds
                    "hd" 'sly-disassemble-symbol
                    "hh" 'sly-describe-symbol
                    "hH" 'sly-hyperspec-lookup
                    "hm" 'sly-who-macroexpands
                    "hp" 'sly-apropos-package
                    "hr" 'sly-who-references
                    "hs" 'sly-who-specializes
                    "hS" 'sly-who-sets
                    "h<" 'sly-who-calls
                    "h>" 'sly-calls-who

                    "c" '(:ignore t :which-key "compile")
                    "cc" 'sly-compile-file
                    "cC" 'sly-compile-and-load-file
                    "cf" 'sly-compile-defun
                    "cl" 'sly-load-file
                    "cn" 'sly-remove-notes
                    "cr" 'sly-compile-region

                    "e" '(:ignore t :which-key "eval")
                    "eb" 'sly-eval-buffer
                    "ee" 'sly-eval-last-expression
                    "eE" 'sly-eval-print-last-expression
                    "ef" 'sly-eval-defun
                    "eF" 'slime-undefine-function
                    "er" 'sly-eval-region

                    ;; "m g" 'spacemacs/common-lisp-navigation-transient-state/body
                    "m" '(:ignore t :which-key "macro")
                    "me" 'sly-macroexpand-1
                    "mE" 'sly-macroexpand-all

                    "s" '(:ignore t :which-key "repl")
                    "sc" 'sly-mrepl-clear-repl
                    "si" 'sly
                    "sq" 'sly-quit-lisp
                    "sr" 'sly-restart-inferior-lisp
                    "ss" 'sly-mrepl-sync

                    "S" '(:ignore t :which-key "stickers")
                    "Sb" 'sly-stickers-toggle-break-on-stickers
                    "Sc" 'sly-stickers-clear-defun-stickers
                    "SC" 'sly-stickers-clear-buffer-stickers
                    "Sf" 'sly-stickers-fetch
                    "Sr" 'sly-stickers-replay
                    "Ss" 'sly-stickers-dwim

                    "t" '(:ignore t :which-key "trace")
                    "tt" 'sly-toggle-trace-fdefinition
                    "tT" 'sly-toggle-fancy-trace
                    "tu" 'sly-untrace-all)

(js|keymap-for-mode 'web-mode
                    "E" '(:ignore t :wk "errors")
                    "El" 'web-mode-dom-errors-show

                    "g" '(:ignore t :wk "goto")
                    "gb" 'web-mode-element-beginning
                    "gc" 'web-mode-element-child
                    "gp" 'web-mode-element-parent
                    "gs" 'web-mode-element-sibling-next

                    "h" '(:ignore t :wk "dom")
                    "hp" 'web-mode-dom-xpath

                    "r" '(:ignore t :wk "refactor")
                    "rc" 'web-mode-element-clone
                    "rd" 'web-mode-element-vanish
                    "rk" 'web-mode-element-kill
                    "rr" 'web-mode-element-rename
                    "rw" 'web-mode-element-wrap

                    "z"  'web-mode-fold-or-unfold)

(js|keymap-for-mode 'emacs-lisp-mode
                    "'" 'ielm

                    "c" '(:ignore t :wk "compile")
                    "cc" 'emacs-lisp-byte-compile

                    "e" '(:ignore t :wk "eval")
                    "e$" 'lisp-state-eval-sexp-end-of-line
                    "eb" 'eval-buffer
                    "eC" 'spacemacs/eval-current-form
                    "ee" 'eval-last-sexp
                    "er" 'eval-region
                    "ef" 'eval-defun
                    "el" 'lisp-state-eval-sexp-end-of-line

                    "gG" 'spacemacs/nav-find-elisp-thing-at-point-other-window
                    ","  'lisp-state-toggle-lisp-state

                    "s" '(:ignore t :wk "ielm")
                    "si" 'ielm)

(js|keymap-for-mode 'org-mode
                    "'" 'org-edit-special
                    "c" 'org-capture

                    ;; Clock
                    ;; These keybindings should match those under the "aoC" prefix (below)
                    "C" '(:ignore t :which-key "clocks")
                    "Cc" 'org-clock-cancel
                    "Cd" 'org-clock-display
                    "Ce" 'org-evaluate-time-range
                    "Cg" 'org-clock-goto
                    "Ci" 'org-clock-in
                    "CI" 'org-clock-in-last
                    "Cj" 'org-clock-jump-to-current-clock
                    "Co" 'org-clock-out
                    "CR" 'org-clock-report
                    "Cr" 'org-resolve-clocks

                    "d" '(:ignore t :which-key "dates")
                    "dd" 'org-deadline
                    "ds" 'org-schedule
                    "dt" 'org-time-stamp
                    "dT" 'org-time-stamp-inactive
                    "ee" 'org-export-dispatch
                    "fi" 'org-feed-goto-inbox
                    "fu" 'org-feed-update-all

                    "a" 'org-agenda

                    "p" 'org-priority

                    "T" '(:ignore t :which-key "toggles")
                    "Tc" 'org-toggle-checkbox
                    "Te" 'org-toggle-pretty-entities
                    "Ti" 'org-toggle-inline-images
                    "Tl" 'org-toggle-link-display
                    "Tt" 'org-show-todo-tree
                    "TT" 'org-todo
                    "TV" 'space-doc-mode
                    "Tx" 'org-toggle-latex-fragment

                    ;; More cycling options (timestamps, headlines, items, properties)
                    "L" 'org-shiftright
                    "H" 'org-shiftleft
                    "J" 'org-shiftdown
                    "K" 'org-shiftup

                    ;; Change between TODO sets
                    "C-S-l" 'org-shiftcontrolright
                    "C-S-h" 'org-shiftcontrolleft
                    "C-S-j" 'org-shiftcontroldown
                    "C-S-k" 'org-shiftcontrolup

                    ;; Subtree editing
                    "s" '(:ignore t :which-key "trees/subtrees")
                    "sa" 'org-toggle-archive-tag
                    "sA" 'org-archive-subtree
                    "sb" 'org-tree-to-indirect-buffer
                    "sh" 'org-promote-subtree
                    "sj" 'org-move-subtree-down
                    "sk" 'org-move-subtree-up
                    "sl" 'org-demote-subtree
                    "sn" 'org-narrow-to-subtree
                    "sN" 'widen
                    "sr" 'org-refile
                    "ss" 'org-sparse-tree
                    "sS" 'org-sort

                    ;; tables
                    "t" '(:ignore t :which-key "tables")
                    "ta" 'org-table-align
                    "tb" 'org-table-blank-field
                    "tc" 'org-table-convert
                    "tdc" 'org-table-delete-column
                    "tdr" 'org-table-kill-row
                    "te" 'org-table-eval-formula
                    "tE" 'org-table-export
                    "th" 'org-table-previous-field
                    "tH" 'org-table-move-column-left

                    "ti" '(:ignore t :which-key "insert")
                    "tic" 'org-table-insert-column
                    "tih" 'org-table-insert-hline
                    "tiH" 'org-table-hline-and-move
                    "tir" 'org-table-insert-row

                    "tI" 'org-table-import
                    "tj" 'org-table-next-row
                    "tJ" 'org-table-move-row-down
                    "tK" 'org-table-move-row-up
                    "tl" 'org-table-next-field
                    "tL" 'org-table-move-column-right
                    "tn" 'org-table-create
                    "tN" 'org-table-create-with-table.el
                    "tr" 'org-table-recalculate
                    "ts" 'org-table-sort-lines

                    "tt" '(:ignore t :which-key "toggle")
                    "ttf" 'org-table-toggle-formula-debugger
                    "tto" 'org-table-toggle-coordinate-overlays

                    "tw" 'org-table-wrap-region

                    ;; Source blocks / org-babel
                    "b" '(:ignore t :which-key "babel")
                    "bp" 'org-babel-previous-src-block
                    "bn" 'org-babel-next-src-block
                    "be" 'org-babel-execute-maybe
                    "bo" 'org-babel-open-src-block-result
                    "bv" 'org-babel-expand-src-block
                    "bu" 'org-babel-goto-src-block-head
                    "bg" 'org-babel-goto-named-src-block
                    "br" 'org-babel-goto-named-result
                    "bb" 'org-babel-execute-buffer
                    "bs" 'org-babel-execute-subtree
                    "bd" 'org-babel-demarcate-block
                    "bt" 'org-babel-tangle
                    "bf" 'org-babel-tangle-file
                    "bc" 'org-babel-check-src-block
                    "bj" 'org-babel-insert-header-arg
                    "bl" 'org-babel-load-in-session
                    "bi" 'org-babel-lob-ingest
                    "bI" 'org-babel-view-src-block-info
                    "bz" 'org-babel-switch-to-session
                    "bZ" 'org-babel-switch-to-session-with-code
                    "ba" 'org-babel-sha1-hash
                    "bx" 'org-babel-do-key-sequence-in-edit-buffer
                    ;; "b." 'spacemacs/org-babel-transient-state/body

                    ;; Multi-purpose keys
                    "," 'org-ctrl-c-ctrl-c
                    "*" 'org-ctrl-c-star
                    "-" 'org-ctrl-c-minus
                    "#" 'org-update-statistics-cookies
                    "RET"   'org-ctrl-c-ret
                    "M-RET" 'org-meta-return
                    ;; attachments
                    "A" 'org-attach
                    ;; insertion
                    "i" '(:ignore t :which-key "insert")
                    "id" 'org-insert-drawer
                    "ie" 'org-set-effort
                    "if" 'org-footnote-new
                    "ih" 'org-insert-heading
                    "iH" 'org-insert-heading-after-current
                    ;; "iK" 'spacemacs/insert-keybinding-org
                    "il" 'org-insert-link
                    "in" 'org-add-note
                    "ip" 'org-set-property
                    "is" 'org-insert-subheading
                    "it" 'org-set-tags

                    "x" '(:ignore t :which-key "text")
                    ;; region manipulation

                    ;; "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
                    ;; "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
                    ;; "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
                    "xo" 'org-open-at-point
                    ;; "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
                    ;; "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
                    ;; "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
                    ;; "xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=) )
                    )

(js|keymap-for-mode 'js2-mode
                    "w" 'js2-mode-toggle-warnings-and-errors

                    ;; "h" '(:ignore t :which-key "help")
                    ;; "g" '(:ignore t :which-key "goto")
                    ;; "r" '(:ignore t :which-key "refactor")

                    "z" '(:ignore t :which-key "folding")
                    "zc" 'js2-mode-hide-element
                    "zo" 'js2-mode-show-element
                    "zr" 'js2-mode-show-all
                    "ze" 'js2-mode-toggle-element
                    "zF" 'js2-mode-toggle-hide-functions
                    "zC" 'js2-mode-toggle-hide-comments)

(js|keymap-for-minor-mode 'lsp-mode
                          ;;format
                          ;; "=" '(:ignore t :wk "format")
                          "=" #'lsp-format-buffer

                          ;;goto
                          "g" '(:ignore t :wk "goto")
                          "gt" #'lsp-find-type-definition
                          "gk" #'spacemacs/lsp-avy-goto-word
                          "gK" #'spacemacs/lsp-avy-goto-symbol
                          "ge" #'lsp-ui-flycheck-list
                          "gM" #'lsp-ui-imenu
                          "gi" #'lsp-find-implementation
                          "gd" #'xref-find-definitions
                          "gr" #'xref-find-references
                          "gs" #'lsp-ui-find-workspace-symbol
                          "gp" #'xref-pop-marker-stack

                          "G" '(:ignore t :wk "peek")
                          "Gi" #'lsp-ui-peek-find-implementation
                          "Gd" #'lsp-ui-peek-find-definitions
                          "Gr" #'lsp-ui-peek-find-references
                          "Gs" #'lsp-ui-peek-find-workspace-symbol
                          "Gp" #'lsp-ui-peek-jump-backward
                          "Gn" #'lsp-ui-peek-jump-forward

                          ;;help
                          "h" '(:ignore t :wk "help")
                          "hh" #'lsp-describe-thing-at-point
                          "hs" #'lsp-describe-session
                          
                          ;;jump
                          ;;backend
                          "b" '(:ignore t :wk "backend")
                          "ba" #'lsp-execute-code-action
                          "bc" #'lsp-capabilities
                          "br" #'lsp-restart-workspace

                          ;;refactor
                          "r" '(:ignore t :wk "refactor")
                          "rr" #'lsp-rename

                          ;;toggles
                          "T" '(:ignore t :wk "toggle")
                          "Td" #'lsp-ui-doc-mode
                          "Ts" #'lsp-ui-sideline-mode
                          "Tl" #'lsp-lens-mode)

(js|keymap-for-mode 'typescript-mode

                    "g" '(:ignore t :which-key "goto")
                    "gg" 'lsp-goto-implementation
                    "gt" 'lsp-goto-type-definition
                    "gu" 'xref-find-references

                    "h" '(:ignore t :which-key "help")
                    "hh" 'lsp-describe-thing-at-point
                    "hs" 'lsp-describe-session

                    "r" '(:ignore t :which-key "refactor")
                    "rr" 'lsp-rename)

(defun js|neotree-keybindings ()
  "Define keybindings when working in Neotree"
  (general-nmap neotree-mode-map
    "RET" 'neotree-enter
    "TAB" 'neotree-stretch-toggle
    "q" 'neotree-hide
    "|" 'neotree-enter-vertical-split
    "-" 'neotree-enter-horizontal-split
    "'" 'neotree-quick-look
    "c" 'neotree-create-node
    "C" 'neotree-copy-node
    "d" 'neotree-delete-node
    "gr" 'neotree-refresh
    "H" 'neotree-select-previous-sibling-node
    "j" 'neotree-next-line
    "J" 'neotree-select-down-node
    "k" 'neotree-previous-line
    "K" 'neotree-select-up-node
    "L" 'neotree-select-next-sibling-node
    "q" 'neotree-hide
    "o" 'neotree-enter
    "r" 'neotree-rename-node
    "R" 'neotree-change-root
    "I" 'neotree-hidden-file-toggle))

(defun js|flyspell-mode-toggle ()
  "Toggle flyspell mode."
  (interactive)
  (if flyspell-mode
      (flyspell-mode -1)
    (flyspell-mode 1)))


(provide '+keybindings)
;;; +keybindings.el ends here
