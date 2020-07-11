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

;; Scratch mode inherit from last buffers major mode
(setq doom-scratch-initial-major-mode t)

;; Disable projectile caching when `fd` is present.
;; Taken from issue #3376
(after! projectile
  (setq! projectile-enable-caching (not (executable-find doom-projectile-fd-binary))))

;; Elixir LSP keeps checking project every N seconds otherwise
(setq lsp-enable-file-watchers t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one
      doom-themes-treemacs-theme "doom-colors")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq-default tab-width 2
              indent-tabs-mode nil
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

(use-package! all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

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

(after! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2

        ;; Highlight the current element
        web-mode-enable-current-element-highlight t))

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


(setq ivy-read-action-function #'ivy-hydra-read-action)

;; Stop DOOM from detecting HTML as "large files"
(pushnew! doom-large-file-excluded-modes 'web-mode)
(use-package! web-mode
  ;; :commands web-mode-set-engine
  :config
    (define-advice web-mode-guess-engine-and-content-type (:around (f &rest r) guess-engine-by-extension)
    (if (and buffer-file-name (equal "ex" (file-name-extension buffer-file-name)))
        (progn (setq web-mode-content-type "html")
          (setq web-mode-engine "elixir")
          (web-mode-on-engine-setted))
      (apply f r))))

;; (use-package! mmm-mode
;;   :init (setq mmm-global-mode 'maybe
;;               mmm-parse-when-idle 't
;;               mmm-set-file-name-for-modes '(web-mode)
;;               mmm-submode-decoration-level 0)
;;   :config
;;   (require 'web-mode)




;;   (mmm-add-classes
;;   `((elixir-liveview
;;       :submode web-mode
;;       :face mmm-declaration-submode-face
;;       :front ,liveview-regex-start
;;       :front-offset (end-of-line 1)
;;       :include-front nil
;;       :back ,liveview-regex-end
;;       :include-back nil
;;       :end-not-begin t)))

;;   (mmm-add-mode-ext-class 'elixir-mode nil 'elixir-liveview))

(use-package! polymode
  :mode ("\\.ex\\'" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~L" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode))
  )
(setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))

;; TODO: Get exdoc highlighting working maybe?
;; (define-innermode poly-markdown-exdoc-innermode
;;   :mode 'gfm-mode
;;   :head-matcher (rx "@" (or "moduledoc" "doc") space (= 3 (char "\"'")))
;;   :tail-matcher (rx (= 3 (char "\"'")))
;;   :head-mode 'host
;;   :tail-mode 'host
;;   :allow-nested nil
;;   :keep-in-mode 'host
;;   :fallback-mode 'host)
