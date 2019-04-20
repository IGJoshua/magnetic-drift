;;;; magnetic-drift.asd

(asdf:defsystem #:magnetic-drift
  :description "A tiny game where you pilot a magnetic car around increasingly complex maze-like tracks. Written for the 2019 Lisp Game Jam. "
  :author "Joshua Suskalo <joshua@suskalo.org>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl.sdl2
               #:skitter #:cepl.skitter.sdl2
               #:nineveh #:dirt
               #:rtg-math #:rtg-math.vari
               #:livesupport #:temporal-functions)
  :components ((:file "package")
               (:file "input")
               (:file "events")
               (:file "entities")
               (:file "components")
               (:file "systems")
               (:file "rendering")
               (:file "magnetic-drift")))
