/*
 * $Id$
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.xharbour.org http://www.harbour-project.org
 *
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *    README file explaining howto compile GD
 *
 * See doc/license.txt for licensing terms.
 *
 */

hbgd library is a wrapper around www.libgd.org graphic library

FOLDERS
=======

Folder structure:
  /
  +- doc            : help & license files
  +- include        : gd c include files
  +- samples        : samples folder
  |  |
  |  +- counter     : a sample counter
  |
  +- tests          : test files
     |
     +- digits      : some digits images from 
     |                http://www.digitmania.holowww.com/all.html 
     |                for counter.prg test sample
     +- images_in   : sample images
     +- images_out  : output of test images

COMPILING
=========

Actually platforms supported are:
- Win32 / BCC32
- GNU systems / GCC

to build library on Win32 with BCC use:
   Download bgd.dll from this link:
   http://www.libgd.org/Downloads (Windows DLL)
   and put a copy of it in this folder and in tests folder
then
   make_b32.bat

on GNU system use:
   make install

   ** requirements: gd, gd-devel, libpng, libpng-devel, libjpeg, libjpeg-devel,
                    freetype, freetype-devel, zlib, zlib-devel

DOCUMENTATION
=============

Not yet finished hbgd.txt is the help file.

SAMPLES
=======

For samples look at tests dir.
gdtest.prg      is an API test application
gdtestcls.prg   is a GDImage/GDChart Class test application
test_out.prg    is a sample of a cgi application for windows (I have to complete it).
antialiased.prg shows how apply anti-alias to lines.
animgif.prg     is a sample to create an animated gif.
bartest.prg     is a sample to create barcodes with GD Library.
counter.prg     is sample applications of a graphic web counter.

to compile:
   in Windows/BCC : bldtest.bat <app_without_prg_ext> (for full static)
   in Linux       : . bldtest.sh <app_without_prg_ext>

