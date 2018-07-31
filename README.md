fare-scripts, random personal scripts
=====================================

fare-scripts is a collection of small scripts I use at home.
These scripts are published as examples of how to write scripts in Common Lisp,
but without the ambition of turning them into widely used programs as such.

Any general purpose utilities will be moved from out of this repository
into the [cl-scripting](http://github.com/fare/cl-scripting) project.

NB: If you want to try these scripts, be sure to install this directory
where ASDF 3.1 will find it, e.g. under `~/common-lisp/` or,
if you use [Quicklisp](https://www.quicklisp.org/beta/), under `~/quicklisp/local-projects/`.
Be sure to install all the dependencies, and/or to use Quicklisp.

Contents:

* [fare-scripts.asd](fare-scripts.asd): the `.asd` file,
  which trivially uses package-inferred-system.

* [make-multi.sh](make-multi.sh): a shell script to create a multicall binary
  that contains all the CL scripts I need in a single image, to combine
  fast startup with memory savings. See <http://fare.livejournal.com/184127.html>

* [bazel.lisp](bazel.lisp): trivial functions to work with [bazel](http://bazel.io)

* [commands.lisp](commands.lisp): functions to manage commands in my PATH.

* [edgar.lisp](edgar.lisp): accessing the EDGAR database from the SEC.gov.

* [git.lisp](git.lisp): some utility for git.

* [gutenberg.lisp](gutenberg.lisp): downloading and processing text from Project Gutenberg.

* [languages.lisp](languages.lisp): scripts to bootstrap various programming languages my way.

* [network.lisp](network.lisp): wrappers for network-manager

* [obsolete-shell-aliases.lisp](obsolete-shell-aliases.lisp): functions I don't use any more,
  kept around for archival purposes after conversion from shell to Lisp.

* [random.lisp](random.lisp): functions to randomly generate various passphrases, etc.

* [repl.lisp](repl.lisp): various helpers for the Lisp REPL.

* [rescript.lisp](rescript.lisp): various silly script filters.

* [shell-aliases.lisp](shell-aliases.lisp): various functions that used to be
  shell aliases and are not better written in Lisp.

* [toggle-touchpad.lisp](toggle-touchpad.lisp): a utility to toggle the touchpad on a laptop
  using X-Window.

* [typographie.lisp](typographie.lisp): a filter so my html file abides by French
  typographic standards, using cl-ppcre for regexp replacement (NB: assumes UTF-8).

* [unmime.lisp](unmime.lisp): filter that's semi-useful when processing
  mime files as e.g. decrypted from PGP encrypted mail.

* [vicode.lisp](vicode.lisp): deal with various vietnamese encodings

* [vicode-test.lisp](vicode-test.lisp): mini test-suite for vicode

* [viet-practice.lisp](viet-practice.lisp): memorization exercises for Vietnamese, courtesy of Robert Strandh

* [xrandr.lisp](xrandr.lisp): wrappers around xrandr, notably for dealing with screen orientation.
