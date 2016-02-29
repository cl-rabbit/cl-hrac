(in-package :cl-user)

(defpackage :cl-hrac.system
  (:use :cl :asdf))

(in-package :cl-hrac.system)

(defsystem :cl-hrac
  :version "0.1"
  :description "RabbitMQ Management HTTP API in Common Lisp"  
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :homepage "https://github.com/cl-rabbit/cl-hrac"
  :licence "MIT"
  :depends-on ("alexandria"
               "nibbles"
               "cl-interpol"
               "wu-decimal"
               "local-time"
               "collectors"
               "trivial-utf-8"
               "fast-io"
               "log4cl")
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "package"))))
  :in-order-to ((test-op (test-op cl-hrac.test))))
