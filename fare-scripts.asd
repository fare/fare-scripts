#-asdf3.1 (error "ASDF 3.1 or bust!")

(defsystem "fare-scripts"
  :version "0" ;; not even released
  :description "Various small programs that I write in CL in lieu of shell scripts"
  :license "MIT" ;; also BSD or bugroff
  :author "Francois-Rene Rideau"
  :class :package-inferred-system
  :depends-on ((:version "cl-scripting" "0.1")
               "fare-scripts/random"
               "fare-scripts/rescript"
               "fare-scripts/shell-aliases"
               "fare-scripts/toggle-touchpad"
               "fare-scripts/typographie"
               "fare-scripts/unmime"))
