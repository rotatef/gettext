;;;;  gettext for Common Lisp
;;;;
;;;;  Copyright (C) 2013 Thomas Bakketun <thomas.bakketun@copyleft.no>
;;;;
;;;;  This library is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU Lesser General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this library.  If not, see <http://www.gnu.org/licenses/>.

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
  (print-texts)
  (dolist (*current-locale* '("en" "nn" "pl"))
    (format t "~&~%*current-locale* = ~A~%" *current-locale*)
    (print-texts)
    (format t "~&Metadata:~%~S" (catalog-meta))))
