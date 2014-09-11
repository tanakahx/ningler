#|
  This file is a part of ningler project.
|#

(in-package :cl-user)
(defpackage ningler-asd
  (:use :cl :asdf))
(in-package :ningler-asd)

(defsystem ningler
  :version "0.1"
  :author "Hiroyuki Tanaka (@tanakahx)"
  :license "MIT"
  :depends-on (:ningle
               :dbi
               :cl-emb)
  :components ((:module "src"
                :components
                ((:file "ningler"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op ningler-test))))

(defpackage #:app-config (:export #:*base-directory*))
(defparameter app-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))
