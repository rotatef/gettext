;; Copyright (c) 2013 Thomas Bakketun <thomas.bakketun@copyleft.no>

(defsystem #:gettext
    :name "A port of gettext runtime to Common Lisp"
    :licence "GNU Lesser General Public Licence 3.0"
    :depends-on (:split-sequence :yacc)
    :serial t
    :components ((:file "packages")
                 (:file "plurals-0")
                 (:file "plurals")
		 (:file "gettext")))
