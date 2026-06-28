(ql:quickload "cl-collider")

(in-package :sc-user)
(named-readtables:in-readtable :sc)

(setf *s* (make-external-server "127.0.0.1" :port 44556))
(server-boot *s*)

(jack-connect)

