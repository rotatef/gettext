gettext for Common Lisp
=======================

This is a port of GNU gettext runtime to Common Lisp.

* Implemented in Common Lisp. No need for any C libraries.
* Use GNU gettext tools during development.
* Supports multithreaded applications with different langauge in each thread.
* Transalations can be embedded into compiled application.


Download and installation
-------------------------

The easiest way is to use Quicklisp, see http://www.quicklisp.org/

Get the sources from GitHub:

    cd quicklisp/local-projects
    git clone git://github.com/copyleft/gettext.git
    ln -s gettext/gettext-example .

Then load it:

    (ql:quickload "gettext")

Eventually gettext will become part of quicklisp, and only the last
step will be neccary (unless you need the latest version).


Quick introduction
------------------

gettext comes with a small sample application located in the directory
gettext-example.

Add gettext as a dependency your system definition (gettext-example.asd):

    :depends-on (:gettext)

Setup gettext in your applications package (package.lisp):

    (gettext:setup-gettext #:example "gettext-example")

The first parameter is the name of the package. The second parameter
is the textdomain. Textdomains are namespaces for the translated
text. For most application a single textdomain (with the same name as
the asdf system) will suffice.

Load the translated messages (example.lisp):

    (preload-catalogs #.(asdf:system-relative-pathname :gettext-example "locale/"))

This macro takes a single parameter, the pathname of a directory tree
containing translation catalogs in MO format. The texts are loaded at
compiled time and becomes part of the compiled file, thus the the MO
files are not need at runtime. An alternative approch is to load the
MO files at runtime, see the section "Loading catalogs at runtime".

Set the current locale by binding the special variable GETTEXT:*CURRENT-LOCALE*:

    (setf *current-locale* :nn)

Here the locale is hardcoded to :NN. In a real world application it
would be set to the current users preferred language. If the
application is a multithreaded multiuser application (like most web
applications), dynamicaly bind GETTEXT:*CURRENT-LOCALE* to the logged
in users preferred language.

Mark texts for translation, e.g.:

    (write-line (_ "This is an example gettext program."))

Extract the texts for translations using the xgettext program from GNU
gettext. This step will have to be repeated whenver the texts are
updated in the source code. See update-translations.sh for a script
that automates this job.

Translate the texts. First create a PO file for the langauge, using
the msginit tool. Then you edit this file using e.g. Emacs with
po-mode. The PO file can easly be updated new texts with the help of
the msgmerge tool. See update-translations.sh.

Finally convert the PO files into MO files using msgfmt. See
update-translations.sh for details.


Loading catalogs at runtime
---------------------------

Replace GETTEXT:PRELOAD-CATALOGS with a SETF of the place
GETTEXT:TEXTDOMAINDIR:

    (setf (textdomaindir "gettext-example") (asdf:system-relative-pathname :gettext-example "locale/"))

The catalogs will be loaded as needed and cached until the
appliciatons quits.


http://www.gnu.org/software/gettext

threading - suited for multiuser web applications

translation can be embedded into compiled file

getting
quicklisp
local-projects

not implemented pgettext (contexts)

encoding hardcoded to UTF-8
sbcl dependency, sb-ext:octects-to-string