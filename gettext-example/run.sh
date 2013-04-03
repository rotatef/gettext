#!/bin/sh

sbcl --noinform --eval '(ql:quickload "gettext-example")' --eval '(example:run)' --eval '(quit)'