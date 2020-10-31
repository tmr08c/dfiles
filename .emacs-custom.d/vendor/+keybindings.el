;;;; +keybindings.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(general-define-key :keymaps 'global
                    [remap describe-function] #'helpful-callable
                    [remap describe-command]  #'helpful-command
                    [remap describe-variable] #'helpful-variable
                    [remap describe-key] #'helpful-key)


(general-define-key :states 'insert
                    "C-v" 'cua-paste
                    "C-c" 'cua-copy-region
                    "C-x" 'cua-cut-region
                    "C-z" 'undo-tree-undo
                    "C-Z" 'undo-tree-redo)

(js|global-keymap "'" '(vterm-other-window :wk "open shell")
                  ";" 'eval-expression
                  ;; "`" 'winum-select-window-by-number
                  "0" '( winum-select-window-0-or-10 :wk t )
                  "1" '( winum-select-window-1 :wk t )
                  "2" '( winum-select-window-2 :wk t )
                  "3" '( winum-select-window-3 :wk t )
                  "4" '( winum-select-window-4 :wk t )
                  "5" '( winum-select-window-5 :wk t )
                  "6" '( winum-select-window-6 :wk t )
                  "7" '( winum-select-window-7 :wk t )
                  "8" '( winum-select-window-8 :wk t )
                  "9" '( winum-select-window-9 :wk t )

                  "o" '(:ignore t :wk "Open")
                  "oA" 'org-agenda

                  "oa" '(:ignore t :wk "org agenda")
                  "oaa" 'org-agenda
                  "oat" 'org-todo-list
                  "oam" 'org-tags-view
                  "oav" 'org-search-view

                  "n" '(:ignore t :wk "Notes")
                  "nc" 'org-capture
                  "nl" 'org-store-link

                  "SPC" '(execute-extended-command :wk "M-x")
                  ;; "TAB" '(switch-to-other-buffer :wk "prev buffer")

                  ;; Help bindings
                  "h" '(:ignore t :wk "Help")
                  "hdf" '(describe-function :wk "describe function")
                  "hdm" '(describe-mode :wk "describe modes") ;; TODO: https://framagit.org/steckerhalter/discover-my-major
                  "hds" '(yas-describe-tables :wk "describve snippets")
                  "hdv" '(describe-variable :wk "describe variable")

                  ;; Buffers
                  "b"   '(:ignore t :wk "Buffers")
                  "bb" '(switch-to-buffer :wk "switch buffers")
                  "bl" '(ibuffer-other-window :wk "list buffers")
                  "bn" '(next-buffer :wk "next buffer")
                  "bp" '(previous-buffer :wk "prev buffer")
                  "bd" '((lambda ()
                           (interactive)
                           (kill-buffer (current-buffer)))
                         :wk "close current buffer")
                  "bs" '(create-scratch-buffer
                         :wk "scratch buffer")

                  ;; Files
                  "f"  '(:ignore t :wk "Files")
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
                         :wk "delete file and kill buffer")
                  "ff" '(find-file :wk "find file")
                  "fed" '((lambda ()
                            (interactive)
                            (find-file-existing js|config-file))
                          :wk "open emacs configuration")

                  "ft" 'neotree-toggle

                  ;; Docs
                  "d" '(:ignore t :wk "Docs")

                  ;; Go to
                  "g" '(:ignore t :wk "Go to")
                  "gd" '(hydra-dumb-jump/body :wk "Dump Jump")
                  ;; "gd" '(dumb-jump-go :wk "definition")
                  ;; "gD" '(dumb-jump-go-other-window :wk "definition (other window)")

                  "hh" '(:ignore t :wk "helpful")
                  "hhh" 'helpful-at-point
                  "hhc" 'helpful-command
                  "hhf" 'helpful-callable
                  "hhk" 'helpful-key
                  "hhm" 'helpful-macro
                  "hhv" 'helpful-variable

                  ;; Layouts
                  ;; TODO remove if we prefer perspective
                  ;; "l" '(eyebrowse-hydra/body :wk "Layouts")
                  "l" '(perspective-hydra/body :wk "Layouts")

                  ;; Project
                  "p"  '(:ignore t :wk "Projects")
                  "p!" '(projectile-run-shell-command-in-root :wk "run command")
                  "p%" '(projectile-replace-regexp :wk "replace regexp")
                  "pa" '(projectile-add-known-project :wk "add project")
                  ;; "p a" '(projectile-toggle-between-implementation-and-test :wk "toggle test")
                  "pc" 'org-projectile-projectile-project-todo-completing-read
                  "pI" '(projectile-invalidate-cache :wk "clear cache")
                  "pR" '(projectile-replace :wk "replace")
                  "pk" '(projectile-kill-buffers :wk "kill buffers")
                  "pr" '(projectile-recentf :wk "recent files")
                  "pb" '(projectile-switch-to-buffer :wk "switch to buffer")
                  "pd" '(projectile-find-dir :wk "find directory")
                  "pf" '(projectile-find-file :wk "open file")
                  "pp" '(projectile-persp-switch-project :wk "open project")
                  "ps" '(counsel-projectile-rg :wk "search in project")
                  "pt" 'neotree-projectile-action

                  ;; Quit
                  "q"  '(:ignore t :wk "Quit")
                  "qq" '(kill-emacs :wk "quit")
                  "qr" '(restart-emacs :wk "restart")

                  ;; Search
                  "s" '(:ignore t :wk "Search")
                  "ss" '(swiper :wk "search buffer")
                  "sS" '(lambda ()
                         (interactive)
                         (let ((input (if (region-active-p)
                                          (buffer-substring-no-properties
                                           (region-beginning) (region-end))
                                        (thing-at-point 'symbol t))))
                           (swiper input))
                         :wk "search buffer")



                  ;; Toggle
                  "t" '(:ignore t :wk "Toggles")
                  "tc" 'hide/show-comments-toggle
                  "th" '(:ignore t :wk "highlight")
                  "thi" 'highlight-indentation-mode
                  "thc" 'highlight-indentation-current-column-mode
                  "tS" 'js|flyspell-mode-toggle
                  "ti" 'indent-guide-mode
                  "t TAB" 'indent-guide-global-mode

                  "u" 'universal-argument-more

                  ;; Windows
                  "w"   '(:ignore t :wk "Windows")
                  "wd" '(delete-window :wk "close window")
                  "w/" '((lambda ()
                           (interactive)
                           (split-window-horizontally)
                           (other-window 1))
                         :wk "split vertical")
                  "w-" '((lambda ()
                           (interactive)
                           (split-window-vertically)
                           (other-window 1))
                         :wk "split horizontal")
                  "wh" '(evil-window-left :wk "window left")
                  "w<left>" '(evil-window-left :wk nil)
                  "wj" '(evil-window-down :wk "window down")
                  "w<down>" '(evil-window-down :wk nil)
                  "wk" '(evil-window-up :wk "window up")
                  "w<up>" '(evil-window-up :wk nil)
                  "wl" '(evil-window-right :wk "window right")
                  "w<right>" '(evil-window-right :wk nil)
                  "w=" '(balance-windows :wk "balance window split")

                  "x" '(:ignore t :wk "text")

                  "xs" '(:ignore t :wk "Spelling")
                  "xsb" 'flyspell-buffer
                  "xsn" 'flyspell-goto-next-error

                  "xg" '(:ignore t :wk "google translate")
                  "xgl" 'spacemacs/set-google-translate-languages
                  "xgQ" 'google-translate-query-translate-reverse
                  "xgq" 'google-translate-query-translate
                  "xgT" 'google-translate-at-point-reverse
                  "xgt" 'google-translate-at-point

                  "xt" '(:ignore t :wk "transpose"))


(js|keymap-for-mode 'pdf-view
                    ;; Slicing image
                    "sm" 'pdf-view-set-slice-using-mouse
                    "sb" 'pdf-view-set-slice-from-bounding-box
                    "sr" 'pdf-view-reset-slice
                    ;; Annotations
                    "a" '(:ignore t :wk "annotations")
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
                    "f" '(:ignore t :wk "fit")
                    "fw" 'pdf-view-fit-width-to-window
                    "fh" 'pdf-view-fit-height-to-window
                    "fp" 'pdf-view-fit-page-to-window
                    ;; Other
                    "s" '(:ignore t :wk "slice/search")
                    "ss" 'pdf-occur
                    "p" 'pdf-misc-print-document
                    "O" 'pdf-outline
                    "n" 'pdf-view-midnight-minor-mode)

(js|keymap-for-mode 'go-mode
                    "t" '(:ignore t :wk "test")
                    "ta" '(js|go-run-test-current-suite :wk "run suite")
                    "tt" '(js|go-run-test-current-function :wk "run current function")

                    "tg" '(:ignore t :wk "generate")
                    "tgf" '(go-gen-test-exported :wk "all exported functions")
                    "tga" '(go-gen-test-all :wk "all functions")
                    "tgs" '(go-gen-test-dwim :wk "selected region")

                    ;; Go To
                    "g" '(:ignore t :wk "goto")
                    "gc" '(go-coverage :wk "coverage")

                    ;; Imports
                    "i" '(:ignore t :wk "imports")
                    "ia" '(go-import-add :wk "add")
                    "ig" '(go-import-add :wk "goto")
                    "ir" '(go-remove-unused-imports :wk "remove unused")

                    ;; Execute
                    "x" '(:ignore t :wk "execute")
                    "xx" '(js|go-run-main :wk "run main")

                    ;; Refactoring
                    "r" '(:ignore t :wk "refactoring")
                    "ri" '(go-impl :wk "implement interface")
                    "rs" '(go-fill-struct :wk "fill struct")
                    "rd" '(godoctor-godoc :wk "godoc")
                    "re" '(godoctor-extract :wk "extract")
                    "rn" '(godoctor-rename :wk "rename")
                    ;; "rN" '(go-rename :wk "rename")
                    "rt" '(godoctor-toggle :wk "toggle")

                    ;; Help
                    "h" '(:ignore t :wk "help")
                    "hh" '(godoc-at-point :wk "godoc at point"))

(js|keymap-for-mode 'json-mode
                    "=" 'spacemacs/json-reformat-dwim)

(js|keymap-for-mode 'adoc-mode
                    "h" '(:ignore t :wk "headers")
                    "h1" 'tempo-template-adoc-title-1
                    ;; Alternative method of inserting top-level heading
                    "hI" 'tempo-template-adoc-title-1
                    "h2" 'tempo-template-adoc-title-2
                    ;; Alternative method of inserting the most usual heading
                    "hi" 'tempo-template-adoc-title-2
                    "h3" 'tempo-template-adoc-title-3
                    "h4" 'tempo-template-adoc-title-4
                    "h5" 'tempo-template-adoc-title-5

                    "x" '(:ignore t :wk "text")
                    "xb" 'tempo-template-adoc-strong
                    "xi" 'tempo-template-adoc-emphasis

                    "p" 'adoc-promote
                    "d" 'adoc-demote)

(js|keymap-for-mode 'elm-mode
                    "'"  'elm-repl-load
                    ;; format
                    "=b" 'elm-mode-format-buffer

                    "c" '(:ignore t :wk "compile")
                    "cb" 'elm-compile-buffer
                    ;; "cB" 'spacemacs/elm-compile-buffer-output
                    "cm" 'elm-compile-main

                    "h" '(:ignore t :wk "help")
                    "hh" 'elm-oracle-doc-at-point
                    "ht" 'elm-oracle-type-at-point
                    ;; refactoring
                    "r" '(:ignore t :wk "refactor")
                    "ri" 'elm-sort-imports

                    "s" '(:ignore t :wk "repl")
                    "si" 'elm-repl-load
                    "sf" 'elm-repl-push-decl
                    ;; "sF" 'spacemacs/elm-repl-push-decl-focus
                    "sr" 'elm-repl-push
                    ;; "sR" 'spacemacs/elm-repl-push-focus

                    "R" '(:ignore t :wk "reactor")
                    "Rn" 'elm-preview-buffer
                    "Rm" 'elm-preview-main

                    "p" '(:ignore t :wk "package")
                    "pi" 'elm-import
                    "pc" 'elm-package-catalog
                    "pd" 'elm-documentation-lookup

                    "t" '(:ignore t :wk "test")
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
                    "c" '(:ignore t :wk "compile")
                    "cp" 'merlin-project-check
                    "cv" 'merlin-goto-project-file
                    "e" '(:ignore t :wk "errors/eval")
                    "eC" 'merlin-error-check
                    "en" 'merlin-error-next
                    "eN" 'merlin-error-prev

                    "g" '(:ignore t :wk "goto")
                    "gb" 'merlin-pop-stack
                    "gg" 'merlin-locate
                    ;; "gG" 'spacemacs/merlin-locate-other-window
                    "gl" 'merlin-locate-ident
                    "gi" 'merlin-switch-to-ml
                    "gI" 'merlin-switch-to-mli
                    "go" 'merlin-occurrences

                    "h" '(:ignore t :wk "help")
                    "hh" 'merlin-document
                    "ht" 'merlin-type-enclosing
                    "hT" 'merlin-type-expr

                    "r" '(:ignore t :wk "refactor")
                    "rd" 'merlin-destruct)

(js|keymap-for-mode 'css-mode
                    "l" 'counsel-css)

(js|keymap-for-mode 'ruby-mode
                    "b" '(:ignore t :wk "bundle")
                    "bc" 'bundle-check
                    "bi" 'bundle-install
                    "bs" 'bundle-console
                    "bu" 'bundle-update
                    "bx" 'bundle-exec
                    "bo" 'bundle-open

                    "fh" 'ruby-hash-syntax-toggle

                    "rr" '(:ignore t :wk "rubocop")
                    "rrd" 'rubocop-check-directory
                    "rrD" 'rubocop-autocorrect-directory
                    "rrf" 'rubocop-check-current-file
                    "rrF" 'rubocop-autocorrect-current-file
                    "rrp" 'rubocop-check-project
                    "rrP" 'rubocop-autocorrect-project

                    "rR" '(:ignore t :wk "refactor")
                    "rRm" 'ruby-refactor-extract-to-method
                    "rRv" 'ruby-refactor-extract-local-variable
                    "rRc" 'ruby-refactor-extract-constant
                    "rRl" 'ruby-refactor-extract-to-let

                    "s" '(:ignore t :wk "repl")
                    "sb" 'ruby-send-buffer
                    "sB" 'ruby-send-buffer-and-go
                    "sf" 'ruby-send-definition
                    "sF" 'ruby-send-definition-and-go
                    "sl" 'ruby-send-line
                    "sL" 'ruby-send-line-and-go
                    "sr" 'ruby-send-region
                    "sR" 'ruby-send-region-and-go
                    "ss" 'ruby-switch-to-inf

                    "t" '(:ignore t :wk "test")
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

                    "T" '(:ignore t :wk "toggle")
                    "T'" 'ruby-toggle-string-quotes
                    "T{" 'ruby-toggle-block)

(js|keymap-for-mode 'lisp-mode
                    "'" 'sly

                    "h" '(:ignore t :wk "help")
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

                    "c" '(:ignore t :wk "compile")
                    "cc" 'sly-compile-file
                    "cC" 'sly-compile-and-load-file
                    "cf" 'sly-compile-defun
                    "cl" 'sly-load-file
                    "cn" 'sly-remove-notes
                    "cr" 'sly-compile-region

                    "e" '(:ignore t :wk "eval")
                    "eb" 'sly-eval-buffer
                    "ee" 'sly-eval-last-expression
                    "eE" 'sly-eval-print-last-expression
                    "ef" 'sly-eval-defun
                    "eF" 'slime-undefine-function
                    "er" 'sly-eval-region

                    ;; "m g" 'spacemacs/common-lisp-navigation-transient-state/body
                    "m" '(:ignore t :wk "macro")
                    "me" 'sly-macroexpand-1
                    "mE" 'sly-macroexpand-all

                    "s" '(:ignore t :wk "repl")
                    "sc" 'sly-mrepl-clear-repl
                    "si" 'sly
                    "sq" 'sly-quit-lisp
                    "sr" 'sly-restart-inferior-lisp
                    "ss" 'sly-mrepl-sync

                    "S" '(:ignore t :wk "stickers")
                    "Sb" 'sly-stickers-toggle-break-on-stickers
                    "Sc" 'sly-stickers-clear-defun-stickers
                    "SC" 'sly-stickers-clear-buffer-stickers
                    "Sf" 'sly-stickers-fetch
                    "Sr" 'sly-stickers-replay
                    "Ss" 'sly-stickers-dwim

                    "t" '(:ignore t :wk "trace")
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
                    "eE" 'eval-last-sexp
                    "ee" 'eros-eval-last-sexp
                    "er" 'eval-region
                    "eF" 'eros-eval-defun
                    "eF" 'eval-defun
                    "el" 'lisp-state-eval-sexp-end-of-line

                    ","  'lisp-state-toggle-lisp-state

                    "s" '(:ignore t :wk "ielm")
                    "si" 'ielm

                    "t" '(:ignore t :wk "test")
                    "ta" 'overseer-test
                    "tt" 'overseer-test-run-test
                    "tb" 'overseer-test-this-buffer
                    "tf" 'overseer-test-file
                    "tg" 'overseer-test-tags
                    "tp" 'overseer-test-prompt
                    "tA" 'overseer-test-debug
                    "tq" 'overseer-test-quiet
                    "tv" 'overseer-test-verbose
                    "th" 'overseer-help

                    "tq" 'ert)

(js|keymap-for-mode 'org-mode
                    "'" 'org-edit-special
                    "c" 'org-capture

                    ;; Clock
                    ;; These keybindings should match those under the "aoC" prefix (below)
                    "C" '(:ignore t :wk "clocks")
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

                    "d" '(:ignore t :wk "dates")
                    "dd" 'org-deadline
                    "ds" 'org-schedule
                    "dt" 'org-time-stamp
                    "dT" 'org-time-stamp-inactive
                    "ee" 'org-export-dispatch
                    "fi" 'org-feed-goto-inbox
                    "fu" 'org-feed-update-all

                    "a" 'org-agenda

                    "p" 'org-priority

                    "T" '(:ignore t :wk "toggles")
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
                    "s" '(:ignore t :wk "trees/subtrees")
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
                    "t" '(:ignore t :wk "tables")
                    "ta" 'org-table-align
                    "tb" 'org-table-blank-field
                    "tc" 'org-table-convert
                    "tdc" 'org-table-delete-column
                    "tdr" 'org-table-kill-row
                    "te" 'org-table-eval-formula
                    "tE" 'org-table-export
                    "th" 'org-table-previous-field
                    "tH" 'org-table-move-column-left

                    "ti" '(:ignore t :wk "insert")
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

                    "tt" '(:ignore t :wk "toggle")
                    "ttf" 'org-table-toggle-formula-debugger
                    "tto" 'org-table-toggle-coordinate-overlays

                    "tw" 'org-table-wrap-region

                    ;; Source blocks / org-babel
                    "b" '(:ignore t :wk "babel")
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
                    "i" '(:ignore t :wk "insert")
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

                    "x" '(:ignore t :wk "text")
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

(js|keymap-for-mode 'org-agenda-mode
                    "a" 'org-agenda
                    "Cc" 'org-agenda-clock-cancel
                    "Ci" 'org-agenda-clock-in
                    "Co" 'org-agenda-clock-out
                    "dd" 'org-agenda-deadline
                    "ds" 'org-agenda-schedule
                    "ie" 'org-agenda-set-effort
                    "ip" 'org-agenda-set-property
                    "it" 'org-agenda-set-tags
                    "sr" 'org-agenda-refile)


(js|keymap-for-mode 'tex-mode
                    "*"   'LaTeX-mark-section      ;; C-c *
                    "."   'LaTeX-mark-environment  ;; C-c .
                    "c"   'LaTeX-close-environment ;; C-c ]
                    "e"   'LaTeX-environment       ;; C-c C-e
                    "i" '(:ignore t :wk "insert")
                    "ii"   'LaTeX-insert-item       ;; C-c C-j
                    "s"   'LaTeX-section           ;; C-c C-s

                    "f" '(:ignore t :wk "fill")
                    "fe"  'LaTeX-fill-environment  ;; C-c C-q C-e
                    "fp"  'LaTeX-fill-paragraph    ;; C-c C-q C-p
                    "fr"  'LaTeX-fill-region       ;; C-c C-q C-r
                    "fs"  'LaTeX-fill-section      ;; C-c C-q C-s

                    "p" '(:ignore t :wk "preview")
                    "pb"  'preview-buffer
                    "pc"  'preview-clearout
                    "pd"  'preview-document
                    "pe"  'preview-environment
                    "pf"  'preview-cache-preamble
                    "pp"  'preview-at-point
                    "pr"  'preview-region
                    "ps"  'preview-section

                    "xB"  'latex/font-medium
                    "xr"  'latex/font-clear
                    "xfa" 'latex/font-calligraphic
                    "xfn" 'latex/font-normal
                    "xfu" 'latex/font-upright)

(js|keymap-for-mode 'js2-mode
                    "w" 'js2-mode-toggle-warnings-and-errors

                    ;; "h" '(:ignore t :wk "help")
                    ;; "g" '(:ignore t :wk "goto")
                    ;; "r" '(:ignore t :wk "refactor")

                    "z" '(:ignore t :wk "folding")
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

                    "g" '(:ignore t :wk "goto")
                    "gg" 'lsp-goto-implementation
                    "gt" 'lsp-goto-type-definition
                    "gu" 'xref-find-references

                    "h" '(:ignore t :wk "help")
                    "hh" 'lsp-describe-thing-at-point
                    "hs" 'lsp-describe-session

                    "r" '(:ignore t :wk "refactor")
                    "rr" 'lsp-rename)

;; (defun js|neotree-keybindings ()
;;   "Define keybindings when working in Neotree."
;;   (general-nmap neotree-mode-map
;;       "RET" 'neotree-enter
;;       "TAB" 'neotree-stretch-toggle
;;       "q" 'neotree-hide
;;       "|" 'neotree-enter-vertical-split
;;       "-" 'neotree-enter-horizontal-split
;;       "'" 'neotree-quick-look
;;       "c" 'neotree-create-node
;;       "C" 'neotree-copy-node
;;       "d" 'neotree-delete-node
;;       "gr" 'neotree-refresh
;;       "H" 'neotree-select-previous-sibling-node
;;       "j" 'neotree-next-line
;;       "J" 'neotree-select-down-node
;;       "k" 'neotree-previous-line
;;       "K" 'neotree-select-up-node
;;       "L" 'neotree-select-next-sibling-node
;;       "q" 'neotree-hide
;;       "o" 'neotree-enter
;;       "r" 'neotree-rename-node
;;       "R" 'neotree-change-root
;;       "I" 'neotree-hidden-file-toggle))

(defun js|flyspell-mode-toggle ()
  "Toggle flyspell mode."
  (interactive)
  (if flyspell-mode
      (flyspell-mode -1)
    (flyspell-mode 1)))


(provide '+keybindings)
;;; +keybindings.el ends here
