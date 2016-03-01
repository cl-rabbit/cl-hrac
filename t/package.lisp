(in-package :cl-user)

(defpackage :cl-hrac.test
  (:use :cl :prove))

(setf hrac:*connection* (make-instance 'hrac:drakma-connection :spec (hrac:make-connection-spec "http://127.0.0.1:15672/api")))
