#!/bin/zsh
A=(
  cl-launch
  --output $BINDIR/$BINARCH/multi --dump !
  --lisp sbcl
  --quicklisp
  --dispatch-system exscribe --system exscribe/typeset # add-on to exscribe
  --dispatch-system fare-scripts/typographie
  --dispatch-system fare-scripts/toggle-touchpad
  --dispatch-system fare-scripts/unmime
  --dispatch-system tthsum
  --dispatch-system workout-timer
  --system fare-scripts/bazel # The following three register their own functions!
  --system fare-scripts/random
  --system fare-scripts/shell-aliases
  --system-package lisp-stripper --dispatch-entry lispwc
)
$A $@
multi fare-scripts-symlinks
multi help
