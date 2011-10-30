(in-package :asdf)

(defsystem stream-cons
    :description "stream-cons allows you to use lazy-seq"
    :depends-on (iterate)
    :version "0.0.1"
    :author "zeptometer"
    :licence "Public Domain"
    :components ((:file "stream-cons")))
    