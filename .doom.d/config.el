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
;; (setq doom-font (font-spec :family "Cascadia Code" :height 120)
;;       doom-variable-pitch-font (font-spec :family "Ubuntu" :height 120))
;; (setq doom-font (font-spec :family "CaskaydiaCove Nerd Font Mono" :style "book" :height 120)
;;       doom-variable-pitch-font (font-spec :family "Ubuntu" :height 120))
;; :size must be in font-spec for big-font-mode to work
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 13.0)
;;       doom-variable-pitch-font (font-spec :family "Ubuntu" :size 13.0))
(setq doom-font (font-spec :family "MesloLGS NF" :size 13.0)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 13.0))
;; (setq doom-font (font-spec :family "CaskaydiaCove Nerd Font Mono" :style "book" :size 13.0)
;;       doom-variable-pitch-font (font-spec :family "Ubuntu" :size 13.0))

;; Scratch mode inherit from last buffers major mode
(setq doom-scratch-initial-major-mode t)

;; Do not copy to kill ring on visual paste
(after! evil
  (setq! evil-kill-on-visual-paste nil))


(after! projectile
  (setq! projectile-files-cache-expire 5
         projectile-track-known-projects-automatically nil))


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

;; (use-package! all-the-icons-ibuffer
;;   :init (all-the-icons-ibuffer-mode 1))

(after! org
  (setq! org-hide-emphasis-markers t
         org-insert-heading-respect-content t
         org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")))
(use-package! org-autolist :after org)
(use-package! mixed-pitch
  :hook
  ((writeroom-mode org-mode markdown-mode gfm-mode) . mixed-pitch-mode))


; Ruby Mode
;;
;; Solargraph does worse job than format-all-mode
(setq-hook! 'ruby-mode-hook +format-with-lsp nil)

;; Treat underscore is a word character
(add-hook! '(ruby-mode-hook elixir-mode-hook)
  (modify-syntax-entry ?_ "w"))

;; Change alignment
(setq! ruby-align-to-stmt-keywords '(if while unless until begin case for def))

;; RSpec
(after! rspec-mode
  ;; Use spring when pid is present
  (setq rspec-use-spring-when-possible t)
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

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode
            yaml-mode
            ruby-mode))

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

;; (setq ivy-read-action-function #'ivy-hydra-read-action)

;; Stop DOOM from detecting HTML as "large files"
(pushnew! doom-large-file-excluded-modes 'web-mode)
;; (use-package! web-mode
;;   ;; :commands web-mode-set-engine
;;   :config
;;   (define-advice web-mode-guess-engine-and-content-type (:around (f &rest r) guess-engine-by-extension)
;;     (if (and buffer-file-name (equal "ex" (file-name-extension buffer-file-name)))
;;         (progn (setq web-mode-content-type "html")
;;                (setq web-mode-engine "elixir")
;;                (web-mode-on-engine-setted))
;;       (apply f r))))


;; (setq +treemacs-git-mode 'deferred ;; requires Python3)

(after! treemacs
  (setq treemacs-show-hidden-files nil
        treemacs-read-string-input 'from-minibuffer))

(use-package! evil-matchit
  :hook (web-mode . turn-on-evil-matchit-mode))
(use-package! polymode
  :mode ("\\.ex\\'" . poly-elixir-web-mode)
  :init (setq! web-mode-engines-alist '(("elixir" . "\\.ex\\'")))
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
    :innermodes '(poly-liveview-expr-elixir-innermode)))

(after! lsp-ui
  ;; Larger
  (setq lsp-ui-doc-max-height 10
        lsp-ui-doc-max-width 80)

  ;; Enable LSP UI doc with mouse hover
  (setq lsp-ui-doc-enable t
    lsp-ui-doc-show-with-cursor nil
    lsp-ui-doc-show-with-mouse nil))


;; (after! eglot
;;   (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("ccls" "-v=1" "-log-file=/tmp/ccls.log"))))
(after! company
  (setq company-minimum-prefix-length 3
        company-tooltip-limit 8))

(after! lsp-mode
  (use-package! lsp-tailwindcss)

  ;; Add origami with LSP integration
  (use-package! lsp-origami)
  (add-hook! 'lsp-after-open-hook #'lsp-origami-try-enable)


  ;; Elixir LSP keeps checking project every N seconds otherwise
  (setq lsp-enable-file-watchers t)

  (setq +cc-default-header-file-mode 'c++-mode)
  (setq +cc-default-header-file-mode 'c++-mode
        lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"
        lsp-clients-clangd-args '(
                                   "-j=3"
                                   "--background-index"
                                   "--clang-tidy"
                                   "--completion-style=detailed"
                                   ;; "--query-driver=/usr/local/bin/gcc-8"
                                   ;; "--query-driver=/Users/justinsmestad/.espressif/tools/xtensa-esp32-elf/esp-2021r1-8.4.0/xtensa-esp32-elf/bin/xtensa-esp32-elf-gcc"
                                   "--query-driver=/Users/justinsmestad/.espressif/tools/xtensa-esp32-elf/esp-2021r1-8.4.0/xtensa-esp32-elf/bin/xtensa-esp32-elf-g++"
                                   ))
  ;; Prefer clangd
  (after! lsp-clangd (set-lsp-priority! 'clangd 2))

  ;; Enable folding
  (setq lsp-enable-folding t)

  ;; Ignore certain directories to limit file watchers
  (dolist (match
           '("[/\\\\].direnv$"
             "[/\\\\].elixir_ls"
             "[/\\\\]node_modules$"
             "[/\\\\]deps"
             "[/\\\\]build"
             "[/\\\\]_build"))
    (add-to-list 'lsp-file-watch-ignored-directories match)))
;; (after! lsp-mode
;;         (require 'lsp-tailwindcss))
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

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


;; Enable Gravatars when viewing comments
(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(setq auth-sources '("~/.authinfo"))

;; Support dotenv files with ENV names
(add-to-list 'auto-mode-alist `(,(rx ".env" (or ".production" ".test" ".development" ".local") string-end) . sh-mode))
(add-to-list 'auto-mode-alist `(,(rx ".envrc.private" string-end) . sh-mode))

;; Brewfile
(add-to-list 'auto-mode-alist `(,(rx "Brewfile" string-end) . fundamental-mode))

;; Set custom snippets directory
(setq +snippets-dir "~/.config/snippets")

;; Turn off undo-fu-session-mode
(remove-hook! 'undo-fu-mode-hook #'global-undo-fu-session-mode)

(use-package! interleave :defer t)

;; Only check for git. We don't use anything else.
(setq vc-handled-backends '(Git))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! tree-sitter
  :init
  (defface tree-sitter-hl-face:warning
    '((default :inherit font-lock-warning-face))
    "Face for parser errors"
    :group 'tree-sitter-hl-faces)

  (defun hook/tree-sitter-common ()
    (unless font-lock-defaults
      (setq font-lock-defaults '(nil)))
    (setq tree-sitter-hl-use-font-lock-keywords nil)
    (tree-sitter-mode +1)
    (tree-sitter-hl-mode +1))

  (defun hook/elixir-tree-sitter ()
    (require 'f)
    (require 's)

    (setq tree-sitter-hl-default-patterns
     (read
      (concat
       "["
       (s-replace "#match?" ".match?"
                  (f-read-text (expand-file-name "~/code/github/tree-sitter-elixir/queries/highlights.scm")))
       "]")))
    (hook/tree-sitter-common))

  :hook ((elixir-mode . hook/elixir-tree-sitter))
  :custom-face
  (tree-sitter-hl-face:operator ((t)))
  (tree-sitter-hl-face:variable ((t)))
  (tree-sitter-hl-face:function.method.call ((t)))
  (tree-sitter-hl-face:property ((t))))

(use-package! tree-sitter-langs
  :after tree-sitter
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(elixir-mode . elixir)))

;; Fix exit error 2 with projectile-search
;; (after! counsel
;;   (advice-add 'counsel-rg
;;               :around
;;               (lambda (func &rest args)
;;                 (cl-letf (((symbol-function #'process-exit-status)
;;                            (lambda (_proc) 0)))
;;                   (apply func args)))))
