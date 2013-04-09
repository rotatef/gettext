(in-package #:example)

;(setf (textdomaindir "gettext-example") (asdf:system-relative-pathname :gettext-example "locale/"))
;(setf (textdomain) "gettext-example")

(preload-catalogs #.(asdf:system-relative-pathname :gettext-example "locale/"))

(setf *current-locale* "nn")

(defun run ()
  (write-line (_ "This is an example gettext program."))
  (write-line (_ "This text is not translated."))
  (dotimes (i 20)
    (format t (ngettext "I see one dog.~%" "I see ~D dogs.~%" i) i)))
