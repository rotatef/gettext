(defpackage #:gettext
  (:use #:common-lisp)
  (:export
   #:gettext*
   #:ngettext*
   #:gettext-noop
   #:textdomain
   #:textdomaindir
   #:lc-category
   #:preload-catalogs
   #:setup-gettext
   #:*current-locale*))

