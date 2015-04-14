#-asdf3 (error "ASDF 3 or bust!")

(defsystem "fare-scripts"
  :class :package-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("fare-scripts/typographie"
               "fare-scripts/rescript"
               "fare-scripts/toggle-touchpad"
               "fare-scripts/unmime"))
