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

(in-package #:gettext)

(defun parse-plurals-form (string)
  (let (pluralsp npluralsp)
    (unless (when string
              (let ((plural (search "plural=" string))
                    (nplurals (search "nplurals=" string)))
                (when (and plural nplurals)
                  ;; First get the number
                  (incf nplurals 9)
                  (loop while (< nplurals (length string))
                        while (member (char string nplurals) '(#\Page #\Newline #\Return #\Tab #\Vt))
                        do (incf nplurals))
                  (when (char<= #\0 (char string nplurals) #\9)
                    (setf pluralsp (ignore-errors (parse-integer string :start nplurals :junk-allowed t)))
                    (when pluralsp
                      (incf plural 7)
                      (setf npluralsp (subseq string plural)))
                    t))))
      ;; By default we are using the Germanic form: singular form only
      ;; for `one', the plural form otherwise.  Yes, this is also what
      ;; English is using since English is a Germanic language.
      (setf pluralsp 2
            npluralsp "(n != 1)"))
    (values pluralsp npluralsp)))

(defun digitp (c) (member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))

(defun lexer (stream)
  (flet ((next-is (char)
           (eql char (peek-char nil stream nil))))
    (loop for c = (peek-char nil stream nil)
          while (member c '(#\Space #\Tab))
          do (read-char stream))
    (let ((c (read-char stream nil nil)))
      (if (digitp c)
          (let ((buffer (make-array 10 :element-type 'character
                                    :fill-pointer 0)))
            (do ((c c (read-char stream nil nil)))
                ((or (null c) (not (digitp c)))
                 (unless (null c) (unread-char c stream))
                 (values 'int (parse-integer buffer)))
              (vector-push-extend c buffer)))
          (case c
            (#\=
             (if (next-is #\=)
                 (progn (read-char stream)
                        (values '== '==))
                 (values '= '=)))
            (#\!
             (if (next-is #\=)
                 (progn (read-char stream)
                        (values '!= '!=))
                 (values '! '!)))
            (#\&
             (cond ((next-is #\&)
                    (read-char stream)
                    (values '&& '&&))
                   (t (error "Syntax error: ~S" (read-char stream)))))
            (#\|
             (cond ((next-is #\|)
                    (read-char stream)
                    (values '\|\| '\|\|))
                   (t (error "Syntax error: ~S" (read-char stream)))))
            (#\<
             (if (next-is #\=)
                 (progn (read-char stream)
                        (values '<= '<=))
                 (values '< '<)))
            (#\>
             (if (next-is #\=)
                 (progn (read-char stream)
                        (values '>= '>=))
                 (values '> '>)))
            ((#\* #\/ #\% #\+ %\- #\n #\? #\: #\( #\))
             (let ((v (intern (string (char-upcase c)) #.*package*)))
               (values v v)))
            ((#\; #\Newline nil)
             nil)
            (otherwise
             (error "Syntax error: ~S" (read-char stream))))))))

(yacc:define-parser *plural-expression-parser*
  (:start-symbol expression)
  (:terminals (? \: \|\| && == != < > <= >= + - = * / % ! int n |(| |)|))
  (:precedence ((:right !)
                (:left * / %)
                (:left + -)
                (:left < > <= >=)
                (:left == !=)
                (:left &&)
                (:left \|\|)
                (:right ? \:)))
  (expression
   (expression \|\| expression #'op2)
   (expression && expression #'op2)
   (expression == expression #'op2)
   (expression != expression #'op2)
   (expression < expression #'op2)
   (expression > expression #'op2)
   (expression <= expression #'op2)
   (expression >= expression #'op2)
   (expression + expression #'op2)
   (expression - expression #'op2)
   (expression * expression #'op2)
   (expression / expression #'op2)
   (expression % expression #'op2)
   (expression ? alternation #'op2)
   (! expression)
   int
   n
   (|(| expression |)| #'(lambda (a b c) (declare (ignore a c)) b))
   )
  (alternation
   (expression \: expression (lambda (a b c) (declare (ignore b)) (list a c)))))

(defun parse-plural (string)
  (with-input-from-string (in string)
    (yacc:parse-with-lexer (lambda () (lexer in)) *plural-expression-parser*)))

(defun transform (expr)
  `(lambda (n)
     (declare (ignorable n))
     (flet ((nz (x) (if (member x '(0 nil)) 0 1)))
       (declare (ignorable (function nz)))
       (macrolet ((? (test (then else))
                    `(if (plusp (nz ,test)) ,then ,else)))
         ,(sublis '((\|\| . (lambda (x y) (nz (or (plusp (nz x)) (plusp (nz y))))))
                    (&& . (lambda (x y) (nz (and (plusp (nz x)) (plusp (nz y))))))
                    (% . mod)
                    (/ . truncate)
                    (== . =)
                    (! . (lambda (x) (if (nz x) 0 1)))
                    (!= . (lambda (x y) (nz (/= x y))))
                    (< . (lambda (x y) (nz (< x y))))
                    (> . (lambda (x y) (nz (> x y))))
                    (<= . (lambda (x y) (nz (<= x y))))
                    (>= . (lambda (x y) (nz (>= x y)))))
                  expr)))))

(defun compile-plural (expr)
  (compile nil (transform expr)))

;(funcall (compile-plural (parse-plural "(n+3)+n*!n%5?2:n"))
;         5)

;(transform (parse-plural "(n+3)+n*!n%5?2:n"))1

;(funcall (compile-plural (parse-plural "n != 1")) 1)

;(funcall (compile nil `(lambda (n) ,(compile-plural (parse-plural "n != 1"))))
;         1)


;(funcall
; (compile-plural
;  (parse-plural "(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)")
;  )
; 4)
