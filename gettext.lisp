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

(deftype lc-category ()
  '(member
    :LC_ADDRESS
    :LC_ALL
    :LC_COLLATE
    :LC_CTYPE
    :LC_IDENTIFICATION
    :LC_MEASUREMENT
    :LC_MESSAGES
    :LC_MONETARY
    :LC_NAME
    :LC_NUMERIC
    :LC_PAPER
    :LC_TELEPHONE
    :LC_TIME))

(defvar *current-locale* nil)

(defvar *textdomaindirs* (make-hash-table :test #'equal))

(defvar *textdomain* nil)

(defun textdomaindir (domain)
  (check-type domain string)
  (gethash domain *textdomaindirs*))

(defun (setf textdomaindir) (directory domain)
  (check-type domain string)
  (setf (gethash domain *textdomaindirs*) (pathname directory)))

(defun textdomain ()
  *textdomain*)

(defun (setf textdomain) (domain)
  (check-type domain string)
  (setf *textdomain* domain))

(defun catalog-pathname (key)
  (destructuring-bind (locale category domain)
      key
    (check-type locale string)
    (check-type category lc-category)
    (check-type domain string)
    (let ((basedir (gethash domain *textdomaindirs*)))
      (when basedir
        (probe-file
         (merge-pathnames
          (make-pathname :directory `(:relative ,locale ,(symbol-name category))
                         :name domain
                         :type "mo")
          basedir))))))

(defun decode-catalog-pathname (pathname)
  (check-type pathname pathname)
  (assert (<= 3 (length (pathname-directory pathname))) ()
          "The pathname ~S has to few directory components to be a valid catalog pathname."
          pathname)
  (list (car (last (pathname-directory pathname) 2))
        (intern (car (last (pathname-directory pathname))) :keyword)
        (pathname-name pathname)))

(defun read-mo (pathname)
  (let ((original-lengths+offsets)
        (translation-lengths+offsets))
    (with-open-file (in pathname :element-type '(unsigned-byte 32))
      (let ((swap-endianess #'identity)
            (magic (read-byte in)))
        (case magic
          (#x950412de)
          (#xde120495 (setf swap-endianess
                            (lambda (x)
                              (dpb (ldb (byte 8 0) x) (byte 8 24)
                                   (dpb (ldb (byte 8 8) x) (byte 8 16)
                                        (dpb (ldb (byte 8 16) x) (byte  8 8)
                                             (ldb (byte 8 24) x)))))))
          (otherwise (error "Wrong magic in MO file ~S = ~X" pathname magic)))
        (let ((revision (funcall swap-endianess (read-byte in)))
              (num-strings (funcall swap-endianess (read-byte in)))
              (offset-original (funcall swap-endianess (read-byte in)))
              (offset-translation (funcall swap-endianess (read-byte in))))
          (assert (= revision 0))
          (flet ((read-lengths+offsets (offset)
                   (let ((lengths+offsets (make-array (* 2 num-strings)
                                                      :element-type '(unsigned-byte 32))))
                     (file-position in (/ offset 4))
                     (read-sequence lengths+offsets in)
                     (map-into lengths+offsets swap-endianess lengths+offsets)
                     lengths+offsets)))
            (setf original-lengths+offsets (read-lengths+offsets offset-original))
            (setf translation-lengths+offsets (read-lengths+offsets offset-translation))))))
    (with-open-file (in pathname :element-type '(unsigned-byte 8))
      (flet ((read-strings (lengths+offsets)
               (loop for i from 0 below (length lengths+offsets) by 2
                     collect (let ((bytes (make-array (aref lengths+offsets i)
                                                      :element-type '(unsigned-byte 8))))
                               (file-position in (aref lengths+offsets (1+ i)))
                               (read-sequence bytes in)
                               (flex:octets-to-string bytes :external-format :utf-8)))))
        (let ((originals (read-strings original-lengths+offsets))
              (translations (read-strings translation-lengths+offsets))
              (table (make-hash-table :test 'equal)))
          (loop for original in originals
                for translation in translations
                do (setf (gethash (first (null-split original)) table) (null-split translation)))
          table)))))

(defun null-split (string)
  (split-sequence:split-sequence (code-char 0) string))

(defun parse-headers (string)
  (with-input-from-string (in string)
    (loop for line = (read-line in nil nil)
          while line
          for split = (position #\: line)
          collect (cons (intern (string-upcase (subseq line 0 split)) :keyword)
                        (string-trim " " (subseq line (1+ split)))))))

(defstruct catalog
  key
  headers
  nplurals
  plurals-function
  messages)

(defvar *catalog-cache* (make-hash-table :test 'equal))

(defun construct-catalog (key messages)
  (let ((headers (parse-headers (first (gethash "" messages)))))
    (remhash "" messages)
    (multiple-value-bind (nplurals plural-expr)
        (parse-plurals-form (cdr (assoc :plural-forms headers)))
      (make-catalog :key key
                    :headers headers
                    :nplurals nplurals
                    :plurals-function (compile-plural (parse-plural plural-expr))
                    :messages messages))))

(defun define-catalog (key &optional messages)
  (unless messages
    (let ((pathname (catalog-pathname key)))
      (when pathname
        (setf messages (read-mo pathname)))))
    (setf (gethash key *catalog-cache*)
          (when messages
            (construct-catalog key messages))))

(defmacro preload-catalogs (textdomaindir)
  (check-type textdomaindir pathname)
  `(progn
     ,@(loop for file in (directory (merge-pathnames "*/*/*.mo" textdomaindir))
             collect `(define-catalog ',(decode-catalog-pathname file) ,(read-mo file)))))

(defun get-catalog (locale category domain)
  (let ((key (list (or locale *current-locale*)
                   (or category :lc_messages)
                   (or domain (textdomain)))))
    (unless (member nil key)
      (multiple-value-bind (catalog found)
          (gethash key *catalog-cache*)
        (if found
            catalog
            (define-catalog key))))))

(defun catalog-meta* (&optional domain catergory locale)
  (let ((catalog (get-catalog locale catergory domain)))
    (when catalog
      (catalog-headers catalog))))

(defun lookup (msgid domain category locale)
  (let ((catalog (get-catalog locale category domain)))
    (when catalog
      (gethash msgid (catalog-messages catalog)))))

(defun gettext* (msgid &optional domain category locale)
  (or (first (lookup msgid domain category locale))
      msgid))

(defun ngettext* (msgid1 msgid2 n &optional domain category locale)
  (let* ((catalog (get-catalog locale category domain))
         (translation (and catalog (lookup msgid1 domain category locale))))
    (if translation
        (elt translation
             (funcall (catalog-plurals-function catalog) n))
        (if (= 1 n)
            msgid1
            msgid2))))

(defun gettext-noop (msgid)
  msgid)

(defmacro setup-gettext (package default-domain)
  (setf package (find-package package))
  (check-type default-domain string)
  `(progn
     (defun ,(intern "GETTEXT" package) (msgid &optional domain category locale)
       (gettext* msgid (or domain ,default-domain) category locale))
     (defun ,(intern "_" package) (msgid &optional domain category locale)
       (gettext* msgid (or domain ,default-domain) category locale))
     (defun ,(intern "NGETTEXT" package) (msgid1 msgid2 n &optional domain category locale)
       (ngettext* msgid1 msgid2 n (or domain ,default-domain) category locale))
     (defun ,(intern "N_" package) (msgid)
       msgid)
     (defun ,(intern "CATALOG-META" package) (&optional domain category locale)
       (catalog-meta* (or domain ,default-domain) category locale))))
