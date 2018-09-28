;;; core.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defun keymap-for-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under `space-leader-def` for MODE.
MODE should be a quoted symbol corresponding to a valid major mode.
The rest of the arguments are treated exactly like they are in
'general' package."
  (let (mode-bindings)
    (while key
      (push `(quote ,def) mode-bindings)
      (push (concat "m" key) mode-bindings)
      (setq key (pop bindings) def (pop bindings)))
    (eval `(space-leader-def
             :states '(normal visual)
             :keymaps (quote ,(intern (format "%s-map" mode)))
             ,@mode-bindings))) t)

(provide 'core)
;;; core.el ends here
