/*
 * $Id$
 */

Harbour library project template
================================

Build static lib:
   $ hbmk2 hbtpl.hbp

Build dynamic lib:
   $ hbmk2 -hbdyn hbtpl.hbp

Build and run sample and test code using:
   $ cd tests
   $ hbmk2 sample
   $ sample
   $ hbmk2 test hbtest.hbc
   $ test

Run sample and test code using:
   $ hbrun tests/sample.prg
   $ hbrun tests/test.prg

Use lib from command prompt ("dot prompt"):
   $ cd tests
   $ hbrun

   . ? HBTPL_MYCONSTANT
   . ? hbtpl_MyPublicFunction()

[vszakats]
