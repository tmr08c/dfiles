;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Justin Smestad"
      user-mail-address "justin.smestad@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Cascadia Code" :height 120)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :height 120))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq-default tab-width 2
              ;; Use Github as the standard, ref http://hilton.org.uk/blog/source-code-line-length
              fill-column 125)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(after! org
  (setq! org-hide-emphasis-markers t
         org-insert-heading-respect-content t
         org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")))
(use-package! org-autolist :after org)
(use-package! mixed-pitch
  :hook
  ((writeroom-mode org-mode markdown-mode gfm-mode) . mixed-pitch-mode))

(setq! ruby-align-to-stmt-keywords '(if while unless until begin case for def))
(use-package! feature-mode
  :mode (("\\.feature\\'" . feature-mode)))
(after! rspec-mode
  (dolist (mode '(rspec-verifiable-mode-map rspec-mode-map))
    (undefine-key! mode "SPC m t v"))

    (map! :localleader
        :prefix "t"
        :map (rspec-verifiable-mode-map rspec-mode-map)
        "b" #'rspec-verify))

;; Comma shortcut to SPC m
(after! general
  (general-evil-setup)
  (general-vmap "," (general-simulate-key "SPC m"))
  (general-nmap "," (general-simulate-key "SPC m")))

;; Treat underscore is a word character
(add-hook! '(ruby-mode-hook elixir-mode-hook)
  (modify-syntax-entry ?_ "w"))

;; Disable some flycheck checkers
(setq-default flycheck-disabled-checkers '(ruby-rubylint emacs-lisp-checkdoc))

;; Add ElixirLS so Emacs can find it
(add-to-list 'exec-path "~/code/github/elixir-ls/release")

;; Do not show hidden files by default
(setq! neo-show-hidden-files nil)
(after! neotree
  ;; Treat all files with a leading dot as hidden
  (pushnew! neo-hidden-regexp-list "\\`.DS_Store$" "^\\.")
  (undefine-key! 'neotree-mode-map "v" "s" "RET")
  (map! :map neotree-mode-map
    :n "RET" 'neotree-enter
    :n "q" 'neotree-hide
    :n "C" 'neotree-copy-node
    :n "o" 'neotree-enter
    :n "I" 'neotree-hidden-file-toggle))
