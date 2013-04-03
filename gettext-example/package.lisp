(defpackage #:example
  (:use #:common-lisp #:gettext)
  (:export #:run))

(gettext:setup-gettext #:example "gettext-example")
