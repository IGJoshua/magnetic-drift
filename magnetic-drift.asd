;;;; magnetic-drift.asd

(asdf:defsystem #:magnetic-drift
  :description "A tiny game where you pilot a magnetic car around increasingly complex maze-like tracks. Written for the 2019 Lisp Game Jam. "
  :author "Joshua Suskalo <joshua@suskalo.org>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:cepl.sdl2
               #:slynk ; Hacky workaround for livesupport issues
               #:skitter #:cepl.skitter.sdl2
               #:sdl2-ttf #:cepl.sdl2-ttf
               #:nineveh #:dirt
               #:rtg-math #:rtg-math.vari
               #:livesupport #:temporal-functions)
  :components ((:file "cepl-patch")
               (:file "package")
               (:file "input")
               (:file "events")
               (:file "entities")
               (:file "components")
               (:file "systems")
               (:file "rendering")
               (:file "ui")
               (:file "tilemap")
               (:file "physics")
               (:file "player")
               (:file "gameplay")
               (:file "pause-menu")
               (:file "magnetic-drift")))
