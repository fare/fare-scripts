#!/bin/zsh -feu

has_dll () {
    [[ -n "$(ldconfig -p 2> /dev/null | grep -F ${1}.so)" ]]
}

if has_dll libasound && has_dll libvorbis ; then
    workout_timer=(--dispatch-system workout-timer)
else
    workout_timer=
fi

A=(
  cl-launch
  --output ${BINDIR}/${BINARCH}/multi --dump !
  --lisp sbcl
  --quicklisp
  --dispatch-system exscribe --system exscribe/typeset # add-on to exscribe
  --dispatch-system fare-scripts/typographie
  --dispatch-system fare-scripts/toggle-touchpad
  --dispatch-system fare-scripts/unmime
  --dispatch-system tthsum
  ${workout_timer}
  --system fare-scripts # Many of its subsystems register their own functions!
  --system-package lisp-stripper --dispatch-entry lispwc
)
$A $@
multi fare-scripts-symlink
multi help
