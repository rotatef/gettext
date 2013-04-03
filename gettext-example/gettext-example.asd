;; Copyright (c) 2013 Thomas Bakketun <thomas.bakketun@copyleft.no>

(defsystem #:gettext-example
    :name "gettext-example"
    :licence "GNU Lesser General Public Licence 3.0"
    :depends-on (:gettext)
    :serial t
    :components ((:file "package")
                 (:file "example")))
