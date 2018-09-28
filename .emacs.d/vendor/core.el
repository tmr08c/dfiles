;;; core.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defmacro global-keymap (&rest bindings)
  "Add global BINDINGS as key bindings under `space-leader-def`.
All of the arguments are treated exactly like they are in
'general' package."
  `(space-leader-def
     :states 'normal
     ,@bindings))

(defmacro keymap-for-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under `space-leader-def` for MODE.
MODE should be a quoted symbol corresponding to a valid major mode.
The rest of the arguments are treated exactly like they are in
'general' package."
  (let (mode-bindings)
    (while key
      (push def mode-bindings)
      (push (concat "m" key) mode-bindings)
      (setq key (pop bindings) def (pop bindings)))
    `(space-leader-def
       :states '(normal visual)
       :keymaps ',(intern (format "%s-map" (eval mode)))
       ,@mode-bindings)))

(provide 'core)
;;; core.el ends here
