/*
 * $Id$
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.xharbour.org http://www.harbour-project.org
 *
 * Copyright 2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *    README file explaining howto compile FreeImage library
 *
 * See doc/license.txt for licensing terms.
 *
 */

FreeImage Library is a porting to xHarbour of famous FreeImage Project library.

Actually based on FreeImage version 3.8.0
FreeImage Project's web site is http://freeimage.sourceforge.net/

COMPILING
=========

Actually platforms supported are:
- Win32 / BCC32
- GNU systems / GCC

to build library on Win32 with BCC use:
make_b32.bat

on GNU system use:
make install
WARNING: not actually tested

  ** requirements: to build FreeImage library: gcc-c++, libstdc++-devel

DOCUMENTATION
=============

Look at doc folder for help files.
Last FreeImage pdf manual is downloadable from http://freeimage.sourceforge.net/download.html

SAMPLES
=======

For samples look at tests dir.
fitest.prg is an API test application.

NOTES
=====

WARNING: if you are using Windows platform, download the
         FreeImage.dll in tests before use it.

At this time (28/10/2005 CET) it builds on Windows with last CVS.
Not tested on GNU system, but it have to run correctly.


