(use-package go-mode
  :mode "\\.go\\'")
(use-package go-eldoc
  :commands go-eldoc-setup)
(use-package go-projectile
  :hook (go-mode . go-projectile-mode))
(use-package go-gen-test
  :commands (go-gen-test-exported
             go-gen-test-all
             go-gen-test-dwim))
(use-package go-fill-struct
  :commands (go-fill-struct))
(use-package godoctor
  :commands (godoctor-godoc
             godoctor-extract
             godoctor-rename
             godoctor-toggle))
(use-package go-rename
  :commands  go-rename)
(use-package go-impl
  :commands go-impl)

(provide 'golang)
