;;; js-lisp.el --- Part of my Emacs configuration

;;; Commentary:

;;; Code:

(use-package lispy
	:hook ((emacs-lisp-mode
					lisp-interaction-mode
					lisp-mode
					scheme-mode
					clojure-mode) . lispy-mode)
	:config
	(progn
		(defun conditionally-enable-lispy ()
			(when (eq this-command 'eval-expression)
				(lispy-mode 1)))
		(add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)))


(use-package sly
	:hook ((lisp-mode emacs-lisp-mode) . (lambda ()  (sly-setup '(sly-fancy))))
	:defer t
	:custom
	(inferior-lisp-program "sbcl")
	(sly-autodoc-use-multiline t)
	(sly-complete-symbol*-fancy t)
	(sly-kill-without-query-p t)
	(sly-repl-history-remove-duplicates t)
	(sly-repl-history-trim-whitespaces t)
	(sly-net-coding-system 'utf-8-unix)
	:config
	(progn
		(add-to-list 'company-backends 'company-capf)
		;; (add-to-list 'evil-emacs-state-modes 'sly-mrepl-mode) (this one we want evil)
		(add-to-list 'evil-emacs-state-modes 'sly-inspector-mode)
		(add-to-list 'evil-emacs-state-modes 'sly-db-mode)
		(add-to-list 'evil-emacs-state-modes 'sly-xref-mode)
		(add-to-list 'evil-emacs-state-modes 'sly-stickers--replay-mode))
	:general
	(space-leader-def 'normal lisp-mode-map
		"'" 'sly

		"m h" '(:ignore t :which-key "help")
		"m ha" 'sly-apropos
		"m hb" 'sly-who-binds
		"m hd" 'sly-disassemble-symbol
		"m hh" 'sly-describe-symbol
		"m hH" 'sly-hyperspec-lookup
		"m hm" 'sly-who-macroexpands
		"m hp" 'sly-apropos-package
		"m hr" 'sly-who-references
		"m hs" 'sly-who-specializes
		"m hS" 'sly-who-sets
		"m h<" 'sly-who-calls
		"m h>" 'sly-calls-who

		"m c" '(:ignore t :which-key "compile")
		"m cc" 'sly-compile-file
		"m cC" 'sly-compile-and-load-file
		"m cf" 'sly-compile-defun
		"m cl" 'sly-load-file
		"m cn" 'sly-remove-notes
		"m cr" 'sly-compile-region

		"m e" '(:ignore t :which-key "eval")
		"m eb" 'sly-eval-buffer
		"m ee" 'sly-eval-last-expression
		"m eE" 'sly-eval-print-last-expression
		"m ef" 'sly-eval-defun
		"m eF" 'slime-undefine-function
		"m er" 'sly-eval-region

		;; "m g" 'spacemacs/common-lisp-navigation-transient-state/body
		"m m" '(:ignore t :which-key "macro")
		"m me" 'sly-macroexpand-1
		"m mE" 'sly-macroexpand-all

		"m s" '(:ignore t :which-key "repl")
		"m sc" 'sly-mrepl-clear-repl
		"m si" 'sly
		"m sq" 'sly-quit-lisp
		"m sr" 'sly-restart-inferior-lisp
		"m ss" 'sly-mrepl-sync

		"m S" '(:ignore t :which-key "stickers")
		"m Sb" 'sly-stickers-toggle-break-on-stickers
		"m Sc" 'sly-stickers-clear-defun-stickers
		"m SC" 'sly-stickers-clear-buffer-stickers
		"m Sf" 'sly-stickers-fetch
		"m Sr" 'sly-stickers-replay
		"m Ss" 'sly-stickers-dwim

		"m t" '(:ignore t :which-key "trace")
		"m tt" 'sly-toggle-trace-fdefinition
		"m tT" 'sly-toggle-fancy-trace
		"m tu" 'sly-untrace-all))

(use-package sly-mrepl
	:ensure nil ;; built-in sly
	:defines sly-mrepl-mode-map
	:bind
	(:map sly-mrepl-mode-map
				("<up>" . sly-mrepl-previous-input-or-button)
				("<down>" . sly-mrepl-next-input-or-button)
				("<C-up>" . sly-mrepl-previous-input-or-button)
				("<C-down>" . sly-mrepl-next-input-or-button)))

(use-package sly-repl-ansi-color
	:requires sly
	:demand t
	:config (push 'sly-repl-ansi-color sly-contribs))


;; (use-package sly-company
;; 	:requires (company sly))

;; (use-package slime
;; 	:hook lisp-mode
;; 	:defer t
;; 	:custom
;; 	(inferior-lisp-program "sbcl")

;; 	:config
;; 	(require 'slime-fuzzy)
;; 	(slime-setup)
;; 	:general
;; 	(space-leader-def 'normal lisp-mode
;;     "m '" 'slime

;;     "m c" '(:ignore t :which-key "compile")
;;     "m cc" 'slime-compile-file
;;     "m cC" 'slime-compile-and-load-file
;;     "m cl" 'slime-load-file
;;     "m cf" 'slime-compile-defun
;;     "m cr" 'slime-compile-region
;;     "m cn" 'slime-remove-notes

;;     "m e" '(:ignore t :which-key "eval")
;;     "m eb"  'slime-eval-buffer
;;     "m ef"  'slime-eval-defun
;;     "m eF"  'slime-undefine-function
;;     "m ee"  'slime-eval-last-expression
;;     "m er"  'slime-eval-region

;;     "m g" '(:ignore t :which-key "nav")
;;     "m gb"  'slime-pop-find-definition-stack
;;     "m gn"  'slime-next-note
;;     "m gN"  'slime-previous-note

;;     "m h" '(:ignore t :which-key "help")
;;     "m ha"  'slime-apropos
;;     "m hA"  'slime-apropos-all
;;     "m hd"  'slime-disassemble-symbol
;;     "m hh"  'slime-describe-symbol
;;     "m hH"  'slime-hyperspec-lookup
;;     "m hi"  'slime-inspect-definition
;;     "m hp"  'slime-apropos-package
;;     "m ht"  'slime-toggle-trace-fdefinition
;;     "m hT"  'slime-untrace-all
;;     "m h<"  'slime-who-calls
;;     "m h>"  'slime-calls-who
;;     ;; TODO: Add key bindings for who binds/sets globals?
;;     "m hr"  'slime-who-references
;;     "m hm"  'slime-who-macroexpands
;;     "m hs"  'slime-who-specializes

;;     "m m" '(:ignore t :which-key "macro")
;;     "m ma"  'slime-macroexpand-all
;;     "m mo"  'slime-macroexpand-1

;;     "m s" '(:ignore t :which-key "repl")
;;     "m se"  'slime-eval-last-expression-in-repl
;;     "m si"  'slime
;;     "m sq"  'slime-quit-lisp

;;     "m t" '(:ignore t :which-key "toggle")
;; 		"m tf"  'slime-toggle-fancy-trace
;; 		)
;; 	)

;; (use-package slime-company
;; 	:requires (slime company))

;; (use-package auto-compile
;; 	:commands auto-compile-on-save-mode
;;   :custom
;;   (auto-compile-display-buffer nil)
;; 	(auto-compile-use-mode-line nil))

;; (use-package highlight-quoted
;; 	:commands highlight-quoted-mode)

;; (use-package macrostep
;; 	:commands macrostep-expand
;;   ;; :config
;;   ;; (map! :map macrostep-keymap
;;   ;;       :n "RET"    #'macrostep-expand
;;   ;;       :n "e"      #'macrostep-expand
;;   ;;       :n "u"      #'macrostep-collapse
;;   ;;       :n "c"      #'macrostep-collapse

;;   ;;       :n "TAB"    #'macrostep-next-macro
;;   ;;       :n "n"      #'macrostep-next-macro
;;   ;;       :n "J"      #'macrostep-next-macro

;;   ;;       :n "S-TAB"  #'macrostep-prev-macro
;;   ;;       :n "K"      #'macrostep-prev-macro
;;   ;;       :n "p"      #'macrostep-prev-macro

;;   ;;       :n "q"      #'macrostep-collapse-all
;;   ;;       :n "C"      #'macrostep-collapse-all)
;;   ;; ;; `evil-normalize-keymaps' seems to be required for macrostep or it won't
;;   ;; ;; apply for the very first invocation
;; 	;; (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps)
;; 	)

;; (use-package overseer
;; 	:commands overseer-test)

(provide 'js-lisp)

;;; js-lisp.el ends here
