#|
  This file is a part of ningler project.
|#

(in-package :cl-user)
(defpackage ningler-test-asd
  (:use :cl :asdf))
(in-package :ningler-test-asd)

(defsystem ningler-test
  :author ""
  :license ""
  :depends-on (:ningler
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "ningler"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
