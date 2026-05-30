(uiop:define-package :fare-scripts/audio
  (:use :cl :uiop :fare-utils
        :optima :optima.ppcre
        :inferior-shell :cl-scripting :cl-launch/dispatch)
  (:export
   #:volume-status
   #:toggle-volume
   ))

(in-package :fare-scripts/audio)

;;; Sound
(exporting-definitions

(defun volume-status ()
  (uiop:run-program `("pamixer" "--get-volume-human")
                    :input nil :output :string :error-output nil :ignore-error-status t))

(defun toggle-volume ()
  "toggle volume"
  (uiop:run-program `("pamixer" "--toggle-mute")
                    :input nil :output :string :error-output nil :ignore-error-status t)
  (volume-status))

(defun lower-volume ()
  "lower volume"
  (uiop:run-program `("pamixer" "--unmute" "--decrease" "5")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (volume-status))

(defun raise-volume ()
  "raise volume"
  (uiop:run-program `("pamixer" "--unmute" "--increase" "5")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (volume-status))

(defun minimize-volume ()
  "minimize volume"
  (uiop:run-program `("pamixer" "--unmute" "--set-volume" "0")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (volume-status))

(defun maximize-volume ()
  "maximize volume"
  (uiop:run-program `("pamixer" "--unmute" "--set-volume" "100")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (volume-status))

(defun microphone-status () ;; TODO: fix that
  "Get microphone status"
  (uiop:run-program `("pamixer" "--source" "1" "--get-volume-human")
                    :input nil :output :string :error-output nil :ignore-error-status t))

(defun toggle-microphone ()
  "toggle microphone"
  (uiop:run-program `("pamixer" "--source" "1" "--toggle-mute")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (microphone-status))

);exporting-definitions

(register-commands :fare-scripts/audio)
