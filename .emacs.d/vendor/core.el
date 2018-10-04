;;; core.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defmacro global-keymap (&rest bindings)
  "Add global BINDINGS as key bindings under `space-leader-def`.
All of the arguments are treated exactly like they are in
'general' package."
  `(space-leader-def
     :states '(normal emacs)
     ,@bindings))

(defmacro keymap-for-mode (mode key def &rest bindings)
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
       :states 'normal
       :keymaps ',(intern (format "%s-map" (eval mode)))
       ,@mode-bindings)))

(defmacro evil-keymap-for-mode (mode &rest bindings)
  "Add BINDINGS to evil for the provided MODE.
mode should be a quoted symbol corresponding to a valid major mode.
the rest of the arguments are treated exactly like they are in
'general' package."
  `(general-define-key
    :states 'normal
    :keymaps ',(intern (format "%s-map" (eval mode)))
    ,@bindings))



(defun js|smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun js|smartparens-pair-newline-and-indent (id action context)
  (js|smartparens-pair-newline id action context)
  (indent-according-to-mode))


(provide 'core)
;;; core.el ends here
