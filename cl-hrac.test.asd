(in-package :cl-user)

(defpackage :cl-hrac.test.system
  (:use :cl :asdf))

(in-package :cl-hrac.test.system)

(defsystem :cl-hrac.test
  :version "0.1"
  :description "Tests for cl-hrac"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :licence "MIT"
  :depends-on ("cl-bunny"
               "cl-hrac"
               "prove"
               "log4cl"
               "mw-equiv"
               "cl-interpol")
  :serial t
  :components ((:module "t"
                :serial t
                :components
                ((:file "package")
                 (:test-file "dummy")
                 (:test-file "hrac"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
