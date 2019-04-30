@set quickload=(ql:quickload :magnetic-drift)
@set dump=^
(sb-ext:save-lisp-and-die \"magnetic-drift.exe\"^
  :toplevel (lambda () (cepl:repl) (magnetic-drift::run-loop))^
  :executable t^
  :application-type :gui^
  :compression 9)

sbcl --eval "%quickload%" --eval "%dump%"
