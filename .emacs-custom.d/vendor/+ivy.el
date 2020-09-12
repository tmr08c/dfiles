;;; +ivy.el --- Handles all Ivy & Counsel related packages -*- lexical-binding: t; -*-
;;; Summary:

;;; Code:

;; Ivy
;; ------------------------------

;; Counsel
(use-package counsel
  :diminish
  :hook ((ivy-mode . counsel-mode))
  :commands (counsel-M-x
             counsel-find-file
             counsel-descbinds
             counsel-load-theme
             counsel-apropos
             counsel-bookmark
             counsel-faces
             counsel-describe-function
             counsel-describe-variable
             counsel-find-library
             counsel-info-lookup-symbol
             counsel-imenu
             counsel-recentf
             counsel-org-capture
             counsel-grep-or-swiper)
  :config
  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    (when (and (eq system-type 'darwin) (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))

  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")

  (setq counsel-mode-override-describe-bindings t
        counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))


(use-package ivy
  :diminish
  :hook ((after-init . ivy-mode))
  :config
  (setq projectile-completion-system 'ivy)
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function nil
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t))

;; Additional key bindings for Ivy
(use-package ivy-hydra
  :commands ivy-hydra-read-action
  :init (setq ivy-read-action-function #'ivy-hydra-read-action))

(use-package prescient
  :commands prescient-persist-mode
  :init
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :disabled
  :commands ivy-prescient-re-builder
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
  :init
  (defun ivy-prescient-non-fuzzy (str)
    "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  (setq ivy-prescient-retain-classic-highlighting t
        ivy-re-builders-alist
        '((counsel-ag . ivy-prescient-non-fuzzy)
          (counsel-rg . ivy-prescient-non-fuzzy)
          (counsel-pt . ivy-prescient-non-fuzzy)
          (counsel-grep . ivy-prescient-non-fuzzy)
          (counsel-imenu . ivy-prescient-non-fuzzy)
          (counsel-yank-pop . ivy-prescient-non-fuzzy)
          (swiper . ivy-prescient-non-fuzzy)
          (swiper-isearch . ivy-prescient-non-fuzzy)
          (swiper-all . ivy-prescient-non-fuzzy)
          (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
          (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
          (insert-char . ivy-prescient-non-fuzzy)
          (counsel-unicode-char . ivy-prescient-non-fuzzy)
          (t . ivy-prescient-re-builder))
        ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer
               counsel-grep counsel-git-grep counsel-ag counsel-imenu
               counsel-yank-pop counsel-recentf counsel-buffer-or-recentf))

  (ivy-prescient-mode 1))

(use-package lsp-ivy
  :after lsp-mode
  :if (eq +completion-engine 'ivy)
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
              ("C-s-." . lsp-ivy-global-workspace-symbol)))


(use-package ivy-rich
  :hook ((counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))

  (with-no-warnings
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((buffer (get-buffer candidate))
               (buffer-file-name (buffer-file-name buffer))
               (major-mode (buffer-local-value 'major-mode buffer))
               (icon (with-current-buffer buffer (all-the-icons-icon-for-buffer))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((path (concat ivy--directory candidate))
               (file (file-name-nondirectory path))
               (icon (cond
                      ((file-directory-p path)
                       (all-the-icons-icon-for-dir path nil ""))
                      ((string-match "^/.*:$" path)
                       (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
                      ((not (string-empty-p file))
                       (all-the-icons-icon-for-file file :v-adjust -0.05)))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-project-icon (_candidate)
      "Display project icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

    (defun ivy-rich-mode-icon (_candidate)
      "Display mode icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)))

    (defun ivy-rich-function-icon (_candidate)
      "Display function icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple)))

    (defun ivy-rich-variable-icon (_candidate)
      "Display the variable icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue)))

    (defun ivy-rich-symbol-icon (_candidate)
      "Display the symbol icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

    (defun ivy-rich-theme-icon (_candidate)
      "Display the theme icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2)))

    (defun ivy-rich-keybinding-icon (_candidate)
      "Display the keybindings icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "keyboard" :height 0.9 :v-adjust -0.15)))

    (defun ivy-rich-library-icon (_candidate)
      "Display the library icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lblue)))

    (defun ivy-rich-package-icon (_candidate)
      "Display the package icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))

    (defun ivy-rich-font-icon (_candidate)
      "Display the font icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-world-clock-icon (_candidate)
      "Display the world clock icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "globe" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-tramp-icon (_candidate)
      "Display the tramp icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01)))

    (defun ivy-rich-git-branch-icon (_candidate)
      "Display the git branch icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green)))

    (defun ivy-rich-process-icon (_candidate)
      "Display the process icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "bolt" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-imenu-icon (candidate)
      "Display the imenu icon in `ivy-rich'."
      (when (display-graphic-p)
        (let ((case-fold-search nil))
          (cond
           ((string-match-p "Type Parameters?[:)]" candidate)
            (all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
           ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)\\|\\(Parameters?\\)[:)]" candidate)
            (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue))
           ((string-match-p "Constants?[:)]" candidate)
            (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
           ((string-match-p "Enum\\(erations?\\)?[:)]" candidate)
            (all-the-icons-material "storage" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-orange))
           ((string-match-p "References?[:)]" candidate)
            (all-the-icons-material "collections_bookmark" :height 0.95 :v-adjust -0.2))
           ((string-match-p "\\(Types?\\)\\|\\(Property\\)[:)]" candidate)
            (all-the-icons-faicon "wrench" :height 0.9 :v-adjust -0.05))
           ((string-match-p "\\(Functions?\\)\\|\\(Methods?\\)\\|\\(Constructors?\\)[:)]" candidate)
            (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
           ((string-match-p "\\(Class\\)\\|\\(Structs?\\)[:)]" candidate)
            (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
           ((string-match-p "Interfaces?[:)]" candidate)
            (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
           ((string-match-p "Modules?[:)]" candidate)
            (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
           ((string-match-p "Packages?[:)]" candidate)
            (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))
           (t (all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.125))))))

    (when (display-graphic-p)
      (defun my-ivy-rich-bookmark-type (candidate)
        (let ((filename (ivy-rich-bookmark-filename candidate)))
          (cond ((null filename)
                 (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'warning))  ; fixed #38
                ((file-remote-p filename)
                 (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01))
                ((not (file-exists-p filename))
                 (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'error))
                ((file-directory-p filename)
                 (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
                (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
      (advice-add #'ivy-rich-bookmark-type :override #'my-ivy-rich-bookmark-type)))

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          persp-switch-to-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-M-x
          (:columns
           ((ivy-rich-function-icon)
            (counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((ivy-rich-function-icon)
            (counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-set-variable
          (:columns
           ((ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-apropos
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-info-lookup-symbol
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-descbinds
          (:columns
           ((ivy-rich-keybinding-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-dired
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-el
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-fzf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
          counsel-buffer-or-recentf
          (:columns
           ((ivy-rich-file-icon)
            (counsel-buffer-or-recentf-transformer (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info))
           :delimiter "\t")
          counsel-bookmarked-directory
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-package
          (:columns
           ((ivy-rich-package-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-fonts
          (:columns
           ((ivy-rich-font-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-major
          (:columns
           ((ivy-rich-function-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-theme
          (:columns
           ((ivy-rich-theme-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-world-clock
          (:columns
           ((ivy-rich-world-clock-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-tramp
          (:columns
           ((ivy-rich-tramp-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-git-checkout
          (:columns
           ((ivy-rich-git-branch-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-list-processes
          (:columns
           ((ivy-rich-process-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer))
           :delimiter "\t")
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-project-icon)
            (counsel-projectile-find-dir-transformer))
           :delimiter "\t")
          counsel-minor
          (:columns
           ((ivy-rich-mode-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-imenu
          (:columns
           ((ivy-rich-imenu-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          treemacs-projectile
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t"))))

(use-package doom-todo-ivy
	:straight (doom-todo-ivy :type git :host github :repo "jsmestad/doom-todo-ivy")
  :commands doom/ivy-tasks)


(use-package counsel-css
  :commands (counsel-css)
  :hook (css-mode . counsel-css-imenu-setup))

(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

;; Select from xref candidates with Ivy
(use-package ivy-xref
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Correcting words with flyspell via Ivy
(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
              ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package counsel-dash
  :after counsel
  :commands (counsel-dash
             counsel-dash-at-point
             counsel-dash-install-docset)
  :hook
  ((lisp-mode . (lambda () (setq-local counsel-dash-docsets '("Common_Lisp"))))
   (emacs-lisp-mode . (lambda () (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
   (elixir-mode . (lambda () (setq-local counsel-dash-docsets '("Elixir"))))
   (ruby-mode . (lambda () (setq-local counsel-dash-docsets '("Ruby"))))
   (projectile-rails-mode . (lambda () (setq-local counsel-dash-docsets '("Ruby_on_Rails_5"))))
   (sql-mode . (lambda () (setq-local counsel-dash-docsets '("PostgreSQL"))))
   (web-mode . (lambda () (setq-local counsel-dash-docsets '("Javascript" "HTML")))))
  :config
  (setq counsel-dash-browser-func 'eww
        counsel-dash-common-docsets '()
        counsel-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master"))

;; Keymaps
(general-define-key :keymaps 'global
                    [remap apropos]                  'counsel-apropos
                    [remap bookmark-jump]            'counsel-bookmark
                    [remap describe-face]            'counsel-faces
                    [remap describe-function]        'counsel-describe-function
                    [remap describe-variable]        'counsel-describe-variable
                    [remap execute-extended-command] 'counsel-M-x
                    [remap find-file]                'counsel-find-file
                    [remap find-library]             'counsel-find-library
                    [remap info-lookup-symbol]       'counsel-info-lookup-symbol
                    [remap imenu]                    'counsel-imenu
                    [remap recentf-open-files]       'counsel-recentf
                    [remap org-capture]              'counsel-org-capture
                    [remap swiper]                   'counsel-grep-or-swiper
                    [remap projectile-find-file]        'counsel-projectile-find-file
                    [remap projectile-find-dir]         'counsel-projectile-find-dir
                    [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer
                    [remap projectile-grep]             'counsel-projectile-grep
                    [remap projectile-rg]               'counsel-projectile-rg
                    [remap projectile-switch-project]   'counsel-projectile-switch-project
                    [remap switch-to-buffer]       'persp-counsel-switch-buffer
                    ;; [remap persp-switch-to-buffer] '+ivy/switch-workspace-buffer
                    [remap imenu-anywhere]         'ivy-imenu-anywhere)

(js|global-keymap
 "?"   '(counsel-descbinds :wk "Help")
 "dd" '((lambda ()
          (interactive)
          (counsel-dash
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (substring-no-properties (or (thing-at-point 'symbol) "")))))
        :wk "Lookup thing at point")
 "dD" '(counsel-dash :wk "Lookup thing at point with docset")
 "m" '(:ignore t :wk "major mode")
 "feD" '((lambda ()
           (interactive)
           (counsel-find-file user-emacs-directory))
         :wk "search emacs config directory")
 "Ts" '(counsel-load-theme :wk "switch theme")
 "pT" '(doom/ivy-tasks :wk "List project tasks"))

(provide '+ivy)
;;; +ivy.el ends here
