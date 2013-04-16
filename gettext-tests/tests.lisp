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

(in-package #:gettext-tests)

(in-root-suite)
(defsuite gettext-tests)
(in-suite gettext-tests)

(deftest test-setup ()
  (let* ((package-name (gensym "test"))
         (package (make-package package-name :use '(#:common-lisp #:gettext))))
    (eval `(gettext:setup-gettext ,package-name "gettext-tests"))
    (dolist (sym '("GETTEXT" "_" "NGETTEXT"))
      (is (fboundp (find-symbol sym package))))))

(deftest test-plurals ()
  (dolist (test '((nil (1) (0 2))
                  ("" (1) (0 2))
                  ("... junk ..." (1) (0 2))
                  ("nplurals=1; plural=0;" (0 1 3 100))
                  ("nplurals=2; plural=(n != 1);" (1) (0 2))
                  ("nplurals=2; plural=(n > 1);" (0 1) (2 3))
                  ("nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n != 0 ? 1 : 2);" (1 21 31) (11 211) (0))
                  ("nplurals=3; plural=n==1 ? 0 : n==2 ? 1 : 2;" (1) (2) (0 3 4))
                  ("nplurals=3; plural=n==1 ? 0 : (n==0 || (n%100 > 0 && n%100 < 20)) ? 1 : 2;" (1) (0 2 3 19 101 102 119) (20 21 120))
                  ("nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && (n%100<10 || n%100>=20) ? 1 : 2);" (1 21 31) (2 9 22) (10 20))
                  ("nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);" (1 21 31) (2 3 4 22) (10 19))
                  ("nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;" (1) (2 3 4) (5 6))
                  ("nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);" (1) (2 3 4 22) (5 9 25))
                  ("nplurals=4; plural=(n%100==1 ? 0 : n%100==2 ? 1 : n%100==3 || n%100==4 ? 2 : 3);" (1 101) (2 102) (3 4 103 104) (5 105))))
    (multiple-value-bind (nplurals expr)
        (gettext::parse-plurals-form (car test))
      (is (= nplurals (length (cdr test))))
      (let ((plural (gettext::compile-plural (gettext::parse-plural expr))))
        (loop for expect from 0
              for test-n in (cdr test)
              do (dolist (n test-n)
                   (is (= (funcall plural n) expect))))))))
