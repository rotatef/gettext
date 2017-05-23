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

(defsystem #:gettext
    :name "gettext"
    :description "An pure Common Lisp implementation of gettext runtime. gettext is an internationalization and localization (i18n) system commonly used for writing multilingual programs on Unix-like computer operating systems."
    :licence "GNU Lesser General Public Licence 3.0"
    :author "Thomas Bakketun <thomas.bakketun@copyleft.no>"
    :depends-on (:split-sequence :yacc :flexi-streams)
    :serial t
    :components ((:file "packages")
                 (:file "plurals-0")
                 (:file "plurals")
		 (:file "gettext")))

(defmethod perform ((o test-op) (c (eql (find-system '#:gettext))))
  (operate 'load-op '#:gettext-tests)
  (funcall (find-symbol (string :gettext-tests)
                        :gettext-tests)))
