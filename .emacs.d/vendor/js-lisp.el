;;; js-lisp.el --- Part of my Emacs configuration

;;; Commentary:

;;; Code:

(use-package lispy
  :disabled ; quite frustrating library in evil mode
  :custom
  (lispy-close-quotes-at-end-p t)
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
    (add-to-list 'evil-emacs-state-modes 'sly-stickers--replay-mode)
    (defun +common-lisp|cleanup-sly-maybe ()
      "Kill processes and leftover buffers when killing the last sly buffer."
      (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
		       if (and (buffer-local-value 'sly-mode buf)
			       (get-buffer-window buf))
		       return t)
	(dolist (conn (sly--purge-connections))
	  (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
	(let (kill-buffer-hook kill-buffer-query-functions)
	  (mapc #'kill-buffer
		(cl-loop for buf in (delq (current-buffer) (buffer-list))
			 if (buffer-local-value 'sly-mode buf)
			 collect buf)))))

    (defun +common-lisp|init-sly ()
      "Attempt to auto-start sly when opening a lisp buffer."
      (cond ((sly-connected-p))
	    ((executable-find inferior-lisp-program)
	     (let ((sly-auto-start 'always))
	       (sly-auto-start)
	       (add-hook 'kill-buffer-hook #'+common-lisp|cleanup-sly-maybe nil t)))
	    ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
		      inferior-lisp-program))))
    (add-hook 'sly-mode-hook #'+common-lisp|init-sly)

    (defun +common-lisp*refresh-sly-version (version conn)
      "Update `sly-protocol-version', which will likely be incorrect or nil due to
an issue where `load-file-name' is incorrect. Because Doom's packages are
installed through an external script (bin/doom), `load-file-name' is set to
bin/doom while packages at compile-time (not a runtime though)."
      (unless sly-protocol-version
	(setq sly-protocol-version (sly-version nil (locate-library "sly.el"))))
      (advice-remove #'sly-check-version #'+common-lisp*refresh-sly-version))
    (advice-add #'sly-check-version :before #'+common-lisp*refresh-sly-version)
    (keymap-for-mode 'lisp-mode
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
		     "tu" 'sly-untrace-all)))

(use-package sly-mrepl
  :ensure nil ;; built-in sly
  :defines sly-mrepl-mode-map
  :bind
  (:map sly-mrepl-mode-map
	("<up>" . sly-mrepl-previous-input-or-button)
	("<down>" . sly-mrepl-next-input-or-button)
	("<C-up>" . sly-mrepl-previous-input-or-button)
	("<C-down>" . sly-mrepl-next-input-or-button))
  :config
  (with-eval-after-load 'smartparens
    (sp-with-modes '(sly-mrepl-mode)
		   (sp-local-pair "'" "'" :actions nil)
		   (sp-local-pair "`" "`" :actions nil))))

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

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode)
  :commands highlight-quoted-mode)

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
