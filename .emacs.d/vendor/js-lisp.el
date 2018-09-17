;;; js-lisp.el --- Part of my Emacs configuration

;;; Commentary:

;;; Code:

(use-package slime
	:defer t
	:config
  (setq inferior-lisp-program "clisp")
	(require 'slime-fuzzy))

(use-package slime-company
	:requires (slime company))

(use-package auto-compile
	:commands auto-compile-on-save-mode
  :custom
  (auto-compile-display-buffer nil)
	(auto-compile-use-mode-line nil))

(use-package highlight-quoted
	:commands highlight-quoted-mode)

(use-package macrostep
	:commands macrostep-expand
  :config
  (map! :map macrostep-keymap
        :n "RET"    #'macrostep-expand
        :n "e"      #'macrostep-expand
        :n "u"      #'macrostep-collapse
        :n "c"      #'macrostep-collapse

        :n "TAB"    #'macrostep-next-macro
        :n "n"      #'macrostep-next-macro
        :n "J"      #'macrostep-next-macro

        :n "S-TAB"  #'macrostep-prev-macro
        :n "K"      #'macrostep-prev-macro
        :n "p"      #'macrostep-prev-macro

        :n "q"      #'macrostep-collapse-all
        :n "C"      #'macrostep-collapse-all)
  ;; `evil-normalize-keymaps' seems to be required for macrostep or it won't
  ;; apply for the very first invocation
	(add-hook 'macrostep-mode-hook #'evil-normalize-keymaps))

(use-package overseer
	:commands overseer-test)

(provide 'js-lisp)

;;; js-lisp.el ends here
