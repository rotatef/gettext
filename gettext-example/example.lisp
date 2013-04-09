(in-package #:example)

;(setf (textdomaindir "gettext-example") (asdf:system-relative-pathname :gettext-example "locale/"))
;(setf (textdomain) "gettext-example")

(preload-catalogs #.(asdf:system-relative-pathname :gettext-example "locale/"))

(defun print-texts ()
  (write-line (_ "This is an example gettext program."))
  (write-line (_ "This text is not translated."))
  (dotimes (i 33)
    (format t (ngettext "I see one dog.~%" "I see ~D dogs.~%" i) i)))

(defun run ()
  (dolist (*current-locale* '("nn" "pl"))
    (format t "~&~%*current-locale* = ~A~%" *current-locale*)
    (print-texts)))
