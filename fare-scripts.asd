#-asdf3.1 (error "ASDF 3.1 or bust!")

(defsystem "fare-scripts"
  :version "0" ;; not even released
  :description "Various small programs that I write in CL in lieu of shell scripts"
  :license "MIT" ;; also BSD or bugroff
  :author "Francois-Rene Rideau"
  :class :package-inferred-system
  :depends-on ((:version "cl-scripting" "0.1")
               (:version "inferior-shell" "2.0.3.3")
	       (:version "fare-utils" "1.0.0.5")
               "fare-scripts/bazel"
               "fare-scripts/git"
               "fare-scripts/languages"
               "fare-scripts/random"
               "fare-scripts/repl"
               "fare-scripts/rescript"
               "fare-scripts/shell-aliases"
               "fare-scripts/toggle-touchpad"
               "fare-scripts/typographie"
               "fare-scripts/unmime"))
