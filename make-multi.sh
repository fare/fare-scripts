#!/bin/zsh
A=(
  cl-launch
  --output $BINDIR/$BINARCH/multi --dump !
  --lisp sbcl
  --quicklisp
  --dispatch-system asdf-tools
  --dispatch-system exscribe --system exscribe/typeset
  --dispatch-system fare-scripts/typographie
  --dispatch-system fare-scripts/toggle-touchpad
  --dispatch-system fare-scripts/unmime
  --system fare-scripts/shell-aliases # registers its functions!
  --system-package lisp-stripper --dispatch-entry lispwc
  --dispatch-system tthsum
  --dispatch-system workout-timer
)
$A $@
multi fare-scripts-symlinks
