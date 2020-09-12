(use-package ember-mode
  :commands ember-mode)

(use-package ember-yasnippets
  :after ember-mode)


(js|keymap-for-minor-mode 'ember-mode
                          "fi" '(:ignore t :wk "import")
                          "fiu" 'ember-import-upgrade-import-statement-at-point
                          "fie" 'ember-import-from-ember-at-point
                          "fid" 'ember-import-decorator-at-point

                          "fd" '(:ignore t :wk "destroy")
                          "fdp" 'ember-destroy-component
                          "fdc" 'ember-destroy-controller
                          "fdm" 'ember-destroy-model
                          "fdr" 'ember-destroy-route
                          "fdt" 'ember-destroy-template
                          "fdv" 'ember-destroy-view
                          "fdx" 'ember-destroy-mixin
                          "fdi" 'ember-destroy-initializer
                          "fdu" 'ember-destroy-util
                          "fds" 'ember-destroy-service
                          "fdg" 'ember-destroy

                          "ff" '(:ignore t :wk "find")
                          "ffp" 'ember-open-component
                          "ffo" 'ember-open-router
                          "ffc" 'ember-open-controller
                          "ffm" 'ember-open-model
                          "ffr" 'ember-open-route
                          "fft" 'ember-open-template
                          "ffj" 'ember-open-javascript
                          "ffv" 'ember-open-view
                          "ffx" 'ember-open-mixin
                          "ffi" 'ember-open-initializer
                          "ffu" 'ember-open-util
                          "ffs" 'ember-open-service
                          "ffa" 'ember-toggle-addon

                          "fg" '(:ignore t :wk "generate")
                          "fgp" 'ember-generate-component
                          "fgc" 'ember-generate-controller
                          "fgm" 'ember-generate-model
                          "fgr" 'ember-generate-route
                          "fgt" 'ember-generate-template
                          "fgv" 'ember-generate-view
                          "fgx" 'ember-generate-mixin
                          "fgi" 'ember-generate-initializer
                          "fgu" 'ember-generate-util
                          "fgs" 'ember-generate-service
                          "fgg" 'ember-generate

                          "fr" '(:ignore t :wk "run")
                          "frb" 'ember-build
                          "frs" 'ember-serve-or-display
                          "frt" 'ember-test)

(provide 'ember)
