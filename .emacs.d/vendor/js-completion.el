;;; js-completion.el -- Part of my Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;;; Company
;;; Auto-completion framework for most modes
(use-package company
  :defer t
  :delight
  :hook (after-init . global-company-mode)
  :custom
  ;; (company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-code-other-buffers t)
  (company-echo-delay 0) ; remove annoying blinking
  (company-idle-delay 0.6)
  (company-minimum-prefix-length 2)
  (company-require-match 'never)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 14)
  (company-global-modes
   '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode))
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  (company-transformers '(company-sort-by-occurrence))
  (company-backends '()))


(use-package company-async-files
  :defer t
  :no-require t
  :load-path "vendor/"
  :requires company
  :config (add-to-list 'company-backends '(company-keywords company-capf company-async-files)))

(use-package company-box
  :disabled
  :defer t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-purple)
        company-box-icons-elisp
        (list (all-the-icons-material "functions"                        :height 0.8 :face 'all-the-icons-red)
              (all-the-icons-material "check_circle"                     :height 0.8 :face 'all-the-icons-blue)
              (all-the-icons-material "stars"                            :height 0.8 :face 'all-the-icons-orange)
              (all-the-icons-material "format_paint"                     :height 0.8 :face 'all-the-icons-pink))
        company-box-icons-lsp
        '((1  . (all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
          (2  . (all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; method
          (3  . (all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; function
          (4  . (all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; constructor
          (5  . (all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; field
          (6  . (all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
          (7  . (all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
          (8  . (all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
          (9  . (all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
          (10 . (all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
          (11 . (all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
          (12 . (all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
          (13 . (all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
          (14 . (all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
          (15 . (all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
          (16 . (all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
          (17 . (all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
          (18 . (all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
          (19 . (all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
          (20 . (all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
          (21 . (all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
          (22 . (all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
          (23 . (all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
          (24 . (all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
          (25 . (all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))))

  ;; Until sebastiencs/company-box#40 is merged
  (defun +company*box-frontend-even-if-single (command)
    (cond ((eq command 'hide)
           (company-box-hide))
          ((equal company-candidates-length 0)
           (company-box-hide))
          ((eq command 'update)
           (company-box-show))
          ((eq command 'post-command)
           (company-box--post-command))))
  (advice-add #'company-box-frontend :override #'+company*box-frontend-even-if-single))
;; :load-path "vendor/company-box/")

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (prescient-persist-mode +1))

;; (use-package company-quickhelp
;;   :hook (company-mode . company-quickhelp-mode)
;;   :custom
;;   (company-quickhelp-delay 0.1)
;;   :general
;;   (general-def 'insert company-quickhelp-mode-map
;;     "C-k" 'company-select-previous))

(use-package company-flx
  :hook (company-mode . company-flx-mode))

(use-package company-posframe
  :disabled
  :delight
  :hook (company-mode . company-posframe-mode))


;; General
(use-package company-emoji
  :no-require t
  :defer 5
  :hook ((markdown-mode git-commit-mode magit-status-mode magit-log-mode) . (lambda ()
                                                                              (set (make-local-variable 'company-backends) '(company-emoji)))))

;; C/C++
(use-package company-irony
  :no-require t
  :hook irony-mode
  :custom
  (company-irony-ignore-case 'smart))

(use-package company-irony-c-headers
  :no-require t
  :after company-irony
  :hook (irony-mode . (lambda ()
                        (set (make-local-variable 'company-backends) '((company-irony-c-headers company-irony company-etags))))))

;; Python
(use-package company-anaconda
  :no-require t
  :hook (python-mode . (lambda ()
                         (set (make-local-variable 'company-backends) '(company-anaconda)))))

;; Golang
(use-package company-go
  :no-require t
  :load-path "vendor/"
  :hook (go-mode . (lambda ()
                     (set (make-local-variable 'company-backends) '(company-go))))
  :custom
  (company-go-show-annotation t))

;; Shell
(use-package company-shell
  :custom
  (company-shell-delete-duplicates t))



;;; Language Server Mode
(use-package eglot
  :disabled ;; Works but not as good as company-go
  :after company
  :config
  (progn
    (add-to-list
     'eglot-server-programs
     '(go-mode . ("go-langserver" "-gocodecompletion")))))

(use-package lsp-mode
  :disabled ;; TODO: replace with eglot
  :hook prog-mode
  :custom
  (lsp-message-project-root-warning t))

(use-package lsp-ui
  :disabled
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :disabled
  :after (company lsp-mode)
  :custom
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  :config
  (push 'company-lsp company-backends))


(custom-set-faces
 '(company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

(provide 'js-completion)
;;; js-completion.el ends here
