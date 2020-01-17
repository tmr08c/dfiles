(use-package google-translate
  :commands (spacemacs/set-google-translate-languages
             google-translate-query-translate
             google-translate-at-point
             google-translate-query-translate-reverse
             google-translate-at-point-reverse)
  :init
  (progn
    (defun spacemacs/set-google-translate-languages (source target)
      "Set source language for google translate.
For instance pass En as source for English."
      (interactive
       "sEnter source language (ie. en): \nsEnter target language (ie. en): "
       source target)
      (message
       (format "Set google translate source language to %s and target to %s"
               source target))
      (setq google-translate-default-source-language (downcase source))
      (setq google-translate-default-target-language (downcase target))))
  :config
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-enable-ido-completion t
          google-translate-show-phonetic t
          google-translate-default-source-language "en"
          google-translate-default-target-language "de")))


(use-package langtool
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config
  (unless langtool-language-tool-jar
    (setq langtool-language-tool-jar
          (cond ((eq system-type 'darwin)
                 (locate-file "libexec/languagetool-commandline.jar"
                              (js|files-in "/usr/local/cellar/languagetool"
                                           :type 'dirs
                                           :depth 1)))
                ((eq system-type 'linux)
                 "/usr/share/java/languagetool/languagetool-commandline.jar"))
          langtool-mother-tongue "en-US")))

(use-package markdown-mode
  :mode "\\.md$"
  :hook (markdown-mode . flyspell-mode))

;; Spell Checking
(use-package flyspell
  :commands (flyspell-buffer
             flyspell-goto-next-error)
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (not (eq system-type 'windows-nt))
  :commands flyspell-mode
  :hook
  ((text-mode writeroom-mode org-mode markdown-mode gfm-mode) . turn-on-flyspell)
  ;; (prog-mode . flyspell-prog-mode)
  :delight
  :config
  (setq flyspell-issue-message-flag nil
        ;; ispell-silently-savep t
        ispell-program-name (executable-find "aspell")
        ispell-list-command "--list"
        ispell-extra-args '("--sug-mode=ultra"
                            "--lang=en_US"
                            "--dont-tex-check-comments")))

(use-package writeroom-mode
  :commands writeroom-mode)

(use-package pandoc-mode
  :hook ((pandoc-mode . pandoc-load-default-settings)
         (markdown-mode . conditionally-load-default-settings)))

(provide 'write)
